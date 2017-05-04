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
import           Database.Persist.TH
import           GHC.Generics
import           Prelude


data Product = Product {
  productName  :: String,
  productPrice:: Int
}  deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)

data BillCoupon = BillCoupon {
  customer    :: String,
  coupon      :: String,
  productList :: [Product]
} deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)

data CouponResult = Applied Int | Rejected String | Partial String
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data CouponType = ProductFlat Int | CartFlat Int | CartPercent Int
    deriving (Show, Read, Eq, Generic)

couponOption =  defaultOptions { sumEncoding   = ObjectWithSingleField }

instance FromJSON CouponType where
    parseJSON = genericParseJSON couponOption

instance ToJSON CouponType where
    toJSON = genericToJSON couponOption

derivePersistField "CouponType"

prodListEx = [Product {productName = "Water", productPrice = 15}]
billCouponExample = BillCoupon { customer = "test@email.com", coupon = "FLAT100", productList = prodListEx}
