{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Api
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (runStderrLoggingT)
import           Control.Monad.Trans.Reader
import           Data.String.Conversions
import           Data.Text                            (Text, intercalate)
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           SwaggerGen

couponServer :: ConnectionPool -> Server CouponApi
couponServer pool =
  couponAddH :<|> couponGetH :<|> couponDelH
  where
    couponAddH newCoupon = liftIO $ couponAdd newCoupon
    couponGetH code    = liftIO $ couponGet code
    couponDelH code    = liftIO $ couponDel code

    couponAdd :: Coupon -> IO NoContent
    couponAdd newCoupon = flip runSqlPersistMPool pool $ do
      upsert newCoupon [CouponValue =. couponValue newCoupon,
                        CouponMin_price =. couponMin_price newCoupon,
                        CouponValid_from =. couponValid_from newCoupon,
                        CouponValid_till =. couponValid_till newCoupon]
      -- TODO Add more upsert cols
      -- when (isNothing exists) $ void $ insert newCoupon
      return NoContent

    couponGet :: Text -> IO (Maybe Coupon)
    couponGet code = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [CouponCode ==. code] []
      return $ entityVal <$> mUser

    couponDel :: Text -> IO NoContent
    couponDel code = flip runSqlPersistMPool pool $ do
      deleteWhere [CouponCode ==. code]
      return NoContent

billCouponServer :: ConnectionPool -> Server BillCouponApi
billCouponServer pool = liftIO.process
                        where process bill = runSqlPersistMPool (query bill) pool
                              query bill = do mcpn <- selectFirst [CouponCode ==. coupon bill] []
                                              case mcpn of
                                                Just cpn -> computeBill (entityVal cpn) bill
                                                Nothing -> return $ Rejected "Coupon Not Found"

computeBill ::  ( MonadIO m) => Coupon -> BillCoupon -> ReaderT SqlBackend m CouponResult
computeBill c b = do  productLimit <- couponApplicable
                      customerUsageLimit <- custCoupon
                      let (couponWorthy,couponAmount) = couponWorth (couponValue c) b
                      ct <- liftIO getCurrentTime
                      let timeLimit = couponTime ct
                      let validityList = [productLimit,couponUsageLimit,priceLimit,
                                          couponWorthy,customerUsageLimit,timeLimit]
                      let rejectionStrings = ["Coupon not available for product anymore",
                                              "Coupon not available anymore",
                                              "Cart value not enough for coupon",
                                              "Coupon not applicable for cart",
                                              "Coupon usage limit exceeded",
                                              "Coupon Expired/Not yet valid"]
                      let rejLookup = zip validityList rejectionStrings
                      let rsl = map snd $ filter (not.fst) rejLookup
                      let rejectionString = intercalate "\n" rsl
                      liftIO $ mapM_ print rejLookup
                      if and validityList
                        then do upsProdCoupon (couponValue c)
                                upsert (newCustCoupon b) [CustomerCouponCused +=. 1]
                                updateWhere [CouponCode ==. couponCode c] [CouponUsed +=. 1]
                                return $ Applied couponAmount
                        else return $ Rejected rejectionString
                      where couponApplicable = case couponValue c of
                                                    ProductFlat _ -> do pc <-mapM prodCoupon $ productList b
                                                                        return $ and pc
                                                    _ -> return True
                            prodCoupon k = do p <- selectFirst [ProductCouponCode ==. couponCode c,
                                                                ProductCouponProduct ==. productName k] []
                                              return $ prodLimit $ entityVal <$> p
                            custCoupon = do cm <- selectFirst [CustomerCouponCode ==. couponCode c,
                                                               CustomerCouponEmail ==. customer b] []
                                            return $ customerLimit $ entityVal <$> cm
                            upsProdCoupon (ProductFlat cf) = do upsert (newProdCoupon cf b) [ProductCouponPused +=. 1]
                                                                return ()
                            upsProdCoupon _                = return ()
                            priceLimit = couponMin_price c < billAmount b
                            couponUsageLimit = couponUsed c < couponUsage_limit c
                            couponTime ct = couponValid_from c < ct && couponValid_till c > ct
                            prodLimit (Just pc) = productCouponPused pc > couponProduct_limit c
                            prodLimit _ = True
                            customerLimit (Just cp) = customerCouponCused cp > couponCustomer_limit c
                            customerLimit _ = True

billAmount :: BillCoupon -> Int
billAmount b = sum (map productPrice (productList b))

couponWorth :: CouponType -> BillCoupon -> (Bool,Int)
couponWorth (ProductFlat c) b = if productCount == 0
                                      then (False ,0)
                                      else(True ,couponProductDiscount c * productCount)
                                    where matchProducts p = couponProductName c == productName p
                                          productCount = length $ filter matchProducts (productList b)
couponWorth (CartFlat c) b    = (True ,billAmount b - c)
couponWorth (CartPercent c) b = (True ,(billAmount b * c) `div` 100)

swaggerServer :: Server SwaggerApi
swaggerServer = liftIO $ return $ swaggerDoc couponApi

server :: ConnectionPool -> Server ServerApi
server pool = couponServer pool :<|> billCouponServer pool :<|> swaggerServer

app :: ConnectionPool -> Application
app pool = serve couponApi $ couponServer pool :<|> billCouponServer pool

appDebug :: ConnectionPool -> Application
appDebug pool = logStdoutDev $ cors (const $ Just policy) $ serve api $ server pool
            where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkApp :: String -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createPostgresqlPool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

mkDebugPgApp :: String -> IO Application
mkDebugPgApp sqliteFile = do
  pool <- runStderrLoggingT $ createPostgresqlPool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ appDebug pool

mkSqliteApp :: String -> IO Application
mkSqliteApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ appDebug pool

run :: String -> IO ()
run dbConnStr = Warp.run 3000 =<< mkDebugPgApp dbConnStr
