{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Api
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (runStderrLoggingT)
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text
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
      exists <- selectFirst [CouponCode ==. couponCode newCoupon] []
      when (isNothing exists) $ void $ insert newCoupon
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
billCouponServer _ = liftIO.billCouponCompute
                    where billCouponCompute bill = do print bill
                                                      return $ Applied 100

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
run dbConnStr = Warp.run 3000 =<< mkApp dbConnStr
