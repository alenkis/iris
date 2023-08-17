{-# LANGUAGE OverloadedStrings #-}

module DbSpec (spec) where

import           Config                 (Column (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Query (Query))
import           Db                     (columnDefinition)
import           QuoteStr               (quoteStr)
import           Test.Hspec

spec :: Spec
spec = describe "columnDefinition" $ do
    it "returns a correct table definition for a single column" $ do
        let cols = [Column "name" Nothing Nothing]
         in columnDefinition cols `shouldBe` Query "name TEXT"

    it "returns a correct table definition for multiple columns" $ do
        let cols =
                [ Column "id" Nothing Nothing
                , Column "price" Nothing Nothing
                , Column "name" Nothing Nothing
                ]
         in columnDefinition cols `shouldBe` Query "id TEXT, price TEXT, name TEXT"
