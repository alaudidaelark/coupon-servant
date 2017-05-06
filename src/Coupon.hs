{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Coupon where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Swagger
import           Data.Text
import           Database.Persist.TH
import           GHC.Generics
import           Prelude

data Product = Product {
  productName  :: Text,
  productPrice:: Int
}  deriving (Eq, Read, Show, Generic, FromJSON, ToJSON, ToSchema)

data BillCoupon = BillCoupon {
  customer    :: Text,
  coupon      :: Text,
  productList :: [Product]
} deriving (Eq, Read, Show, Generic, FromJSON, ToJSON, ToSchema)

data CouponResult = Applied Int | Rejected String
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema)

data CouponForProduct = CouponForProduct {
    product         ::Text,
    productDiscount ::Int
} deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema)

data CouponType = ProductFlat CouponForProduct | CartFlat Int | CartPercent Int
    deriving (Show, Read, Eq, Generic, ToSchema)

couponOption :: Options
couponOption = defaultOptions { sumEncoding   = ObjectWithSingleField }

instance FromJSON CouponType where
    parseJSON = genericParseJSON couponOption

instance ToJSON CouponType where
    toJSON = genericToJSON couponOption

derivePersistField "CouponType"
