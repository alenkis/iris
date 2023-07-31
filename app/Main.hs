{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli           (Command (TransformCSV), getCliCommand)
import           Config.Decode (Field (..))
import qualified Csv

main :: IO ()
main = do
    (TransformCSV _ filepath) <- getCliCommand
    Csv.filterColumns filepath "./examples/output.csv" fields
    putStrLn "Done."
  where
    fields =
        [ Field{name = "item_group_id", rename = Just "group_id"}
        , Field{name = "price", rename = Nothing}
        , Field{name = "sale_price", rename = Nothing}
        , Field{name = "description", rename = Nothing}
        , Field{name = "product_type", rename = Nothing}
        , Field{name = "mpn", rename = Nothing}
        , Field{name = "gtin", rename = Nothing}
        ]
