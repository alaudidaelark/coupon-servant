{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module SwaggerGen where

import           Api
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types           (camelTo2)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Servant.Swagger

modifier :: String -> String
modifier = drop 1 . dropWhile (/= '_') . camelTo2 '_'

prefixSchemaOptions :: SchemaOptions
prefixSchemaOptions = defaultSchemaOptions { fieldLabelModifier = modifier }

instance ToSchema BillCoupon    where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
instance ToSchema CouponType where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
instance ToSchema Coupon    where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
instance ToSchema Product where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
instance ToSchema Customer     where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
instance ToSchema CouponResult     where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions

swaggerDoc :: Swagger
swaggerDoc = toSwagger api
  & host ?~ "localhost:3000"
  & info.title .~ "Coupon Api"
  & info.version .~ "v1"

genSwaggerDoc :: IO ()
genSwaggerDoc = BL8.writeFile "swagger.json" (encode swaggerDoc)
