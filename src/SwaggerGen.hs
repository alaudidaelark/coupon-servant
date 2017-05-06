{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module SwaggerGen where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types           (camelTo2)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.String.Conversions
import           Data.Swagger
import qualified Data.Text                  as T
import           Models
import           Servant.API
import           Servant.Swagger

modifier :: String -> String
modifier = drop 1 . dropWhile (/= '_') . camelTo2 '_'

prefixSchemaOptions :: SchemaOptions
prefixSchemaOptions = defaultSchemaOptions { fieldLabelModifier = modifier }

instance ToSchema Coupon where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions

swaggerDoc :: HasSwagger api => Proxy api -> Swagger
swaggerDoc api = toSwagger api
  & host ?~ Host {_hostName = "localhost",_hostPort = Just 3000}
  & info.title .~ "Coupon Api"
  & info.version .~ "v1"

