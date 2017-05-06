module Main where

import           App

main :: IO ()
main = run "host=localhost port=5432 user=msfuser dbname=coupon password="

