{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Api
import           Control.Monad.IO.Class
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.String.Conversions
import           Data.Text
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors
import           Servant
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

couponServer :: ConnectionPool -> Server CouponApi
couponServer pool =
  couponAddH :<|> couponGetH :<|> couponDelH
  where
    couponAddH newCoupon = liftIO $ couponAdd newCoupon
    couponGetH code    = liftIO $ couponGet code
    couponDelH code    = liftIO $ couponDel code

    couponAdd :: Coupon -> IO (Maybe Coupon)
    couponAdd newCoupon = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [CouponCode ==. couponCode newCoupon] []
      case exists of
        Nothing -> Just <$> insert newCoupon
        Just _  -> return Nothing
      return Nothing

    couponGet :: Text -> IO (Maybe Coupon)
    couponGet code = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [CouponCode ==. code] []
      return $ entityVal <$> mUser

    couponDel :: Text -> IO (Maybe Coupon)
    couponDel code = flip runSqlPersistMPool pool $ do
      deleteWhere [CouponCode ==. code]
      return Nothing

billCouponServer :: ConnectionPool -> Server BillCouponApi
billCouponServer pool = billCouponComputeH
                        where billCouponComputeH bill = liftIO $ billCouponCompute bill
                                                        -- return $ Applied 100
                              billCouponCompute :: BillCoupon -> IO CouponResult
                              billCouponCompute bill = do putStrLn $ show bill
                                                          return $ Applied 100


server :: ConnectionPool -> Server ServerApi
server pool = couponServer pool :<|> billCouponServer pool


app :: ConnectionPool -> Application
app pool = cors (const $ Just policy) $ serve api $ server pool
            where
               policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkPgApp :: String -> IO Application
mkPgApp sqliteFile = do
  pool <- runStderrLoggingT $ createPostgresqlPool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

mkApp :: String -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: String -> IO ()
run dbConnStr =
  Warp.run 3000 =<< mkPgApp dbConnStr
