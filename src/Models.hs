{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models (module Models,module Coupon) where

import           Coupon
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.TH
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CustomerCoupon
  email Text
  code Text
  cused Int
  UniqueEmail email
  Primary email
  Foreign Coupon fkcoupon code
  deriving Eq Read Show Generic

ProductCoupon
  product Text
  code Text
  pused Int
  UniqueProduct product
  Primary product
  Foreign Coupon fkcoupon code
  deriving Eq Read Show Generic

Coupon json
  code Text
  value  CouponType
  used Int
  valid_from UTCTime default=now()
  valid_till UTCTime default=now()
  min_price Int
  usage_limit Int
  product_limit Int
  customer_limit Int
  UniqueCode code
  Primary code
  deriving Eq Read Show Generic
|]

newProdCoupon :: CouponForProduct -> BillCoupon -> ProductCoupon
newProdCoupon c p = ProductCoupon { productCouponProduct = couponProductName c,
                                    productCouponCode = coupon p,
                                    productCouponPused = 0}

newCustCoupon :: BillCoupon -> CustomerCoupon
newCustCoupon b = CustomerCoupon { customerCouponEmail = customer b,
                                   customerCouponCode = coupon b,
                                   customerCouponCused = 0}

newUCust b = UniqueEmail (customer b)

newUprod c = UniqueProduct (couponProductName c)
