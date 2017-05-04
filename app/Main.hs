module Main where

import           App

import           SwaggerGen

main :: IO ()
main = do genSwaggerDoc
          run "host=localhost port=5432 user=msfuser dbname=coupon password="

-- run "testSql.db"
