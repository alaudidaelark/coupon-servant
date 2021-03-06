{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (module Api,module Models) where

import           Data.Proxy
import           Data.Swagger
import           Data.Text
import           Models
import           Servant.API

type CouponApi =
       "coupon" :> "add" :> ReqBody '[JSON] Coupon :> Post '[JSON] NoContent
  :<|> "coupon" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Coupon)
  :<|> "coupon" :> "del" :> Capture "name" Text  :> Get  '[JSON] NoContent

type BillCouponApi =
       "billcoupon" :> ReqBody '[JSON] BillCoupon :> Post '[JSON] CouponResult

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

type ServerApi = CouponApi :<|> BillCouponApi :<|> SwaggerApi

couponApi :: Proxy (CouponApi :<|> BillCouponApi)
couponApi = Proxy

api :: Proxy ServerApi
api = Proxy
