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
import           Data.Aeson
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.TH
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Customer
  email Text
  UniqueEmail email
  Primary email
  deriving Eq Read Show Generic
-- Product
--   name Text
--   price Int
--   code Text
--   UniqueName name
--   Foreign Coupon fkcoupon code
--   Primary name
--   deriving Eq Read Show Generic
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
