{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Api
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (runStderrLoggingT)
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                            (Text)
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
      -- exists <- selectFirst [CouponCode ==. couponCode newCoupon] []
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

-- computeBillCoupon :: Coupon -> BillCoupon -> CouponResult
-- computeBillCoupon c b = Applied 1000

billCouponServer :: ConnectionPool -> Server BillCouponApi
billCouponServer pool = liftIO.compute
                        where compute bill = runSqlPersistMPool (query bill) pool
                              query bill = do mcpn <- selectFirst [CouponCode ==. coupon bill] []
                                              case mcpn of
                                                Just cpn -> computeBill (entityVal cpn) bill
                                                Nothing -> return $ Rejected "Coupon Not Found"

computeBill ::  ( MonadIO m) => Coupon -> BillCoupon -> ReaderT SqlBackend m CouponResult
computeBill c b = do  pc <- mapM prodCoupon $ productList b
                      cm <- selectFirst [CustomerCouponCode ==. couponCode c,
                                          CustomerCouponEmail ==. customer b] []
                      let mcm = customerLimit $ entityVal <$> cm
                      ct <- liftIO getCurrentTime
                      let valid = couponCount && minLimit && True `elem` pc && mcm && couponTime ct
                      liftIO $ do print pc
                                  print minLimit
                                  print couponCount
                                  print mcm
                                  print $ couponTime ct
                      if valid
                        then return.Applied $ computeDiscountAmount (couponValue c) b
                        else return.Rejected $ "Rejected Coupon Invalid"
                      where prodCoupon k = do p <- selectFirst [ProductCouponCode ==. couponCode c,
                                                                ProductCouponProduct ==. productName k] []
                                              return $ productLimit $ entityVal <$> p
                            minLimit = couponMin_price c < billAmount b
                            couponCount = couponUsed c < couponUsage_limit c
                            couponTime ct = couponValid_from c < ct && couponValid_till c > ct
                            productLimit (Just pc) = productCouponUsage pc > couponProduct_limit c
                            productLimit _ = True
                            customerLimit (Just cp) = customerCouponUsage cp > couponCustomer_limit c
                            customerLimit _ = True

billAmount :: BillCoupon -> Int
billAmount b = sum (map productPrice (productList b))

computeDiscountAmount :: CouponType -> BillCoupon -> Int
computeDiscountAmount (ProductFlat c) b = couponProductDiscount c * productCount
                                          where matchProducts p = couponProductName c == productName p
                                                productCount = length $ filter matchProducts (productList b)
computeDiscountAmount (CartFlat c) b    = billAmount b - c
computeDiscountAmount (CartPercent c) b = (billAmount b * c) `div` 100

productFlat ::  (Monad m) => CouponForProduct -> BillCoupon -> m CouponResult
productFlat p bill = return (Rejected "Coupon ProductFlat Found")

cartFlat ::  (Monad m) => Int -> BillCoupon -> m CouponResult
cartFlat p bill = return (Rejected "Coupon CartFlat Found")

cartPerCent ::  (Monad m) => Int -> BillCoupon -> m CouponResult
cartPerCent p bill = return (Rejected "Coupon CartPercent Found")

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
