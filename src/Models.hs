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
  usage Int
  UniqueEmail email
  Primary email
  Foreign Coupon fkcoupon code
  deriving Eq Read Show Generic

ProductCoupon
  product Text
  code Text
  usage Int
  UniqueProduct product
  Primary product
  Foreign Coupon fkcoupon code
  deriving Eq Read Show Generic

Coupon json
  code Text
  value  CouponType
  min_price Int
  customer_limit Int
  usage_limit Int
  valid_from UTCTime default=now()
  valid_till UTCTime default=now()
  UniqueCode code
  Primary code
  deriving Eq Read Show Generic
|]
