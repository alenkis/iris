{-# LANGUAGE OverloadedStrings #-}

module Config.SimpleSpec (spec) where

import           Data.Maybe             (fromMaybe)
import           GHC.Generics           (Generic)
import           QuoteStr               (quoteStr)

import           Config                 (Config (..), Field (..), Job (..))
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Toml                   (Result (..), decode, encode)
import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue, table, (.=))
import           Toml.ToValue.Generic   (genericToTable)

spec :: Spec
spec =
    do
        let job =
                Job
                    { title = "TOML Example"
                    , groupBy = "item_group_id"
                    , separator = Just ','
                    , field =
                        [ Field
                            { name = "item_group_id"
                            , rename = Just "group_id"
                            }
                        , Field
                            { name = "name"
                            , rename = Nothing
                            }
                        ]
                    }
        let expect = Config{job = job}

        let decodedInput =
                decode
                    [quoteStr|
        [job]
        title = "TOML Example"
        group_by = "item_group_id"
        separator = ","

        [[job.field]]
        name = "item_group_id"
        rename = "group_id"

        [[job.field]]
        name = "name"
        |]

        describe "simple config" $ do
            it "should decode successfully" $ do
                decodedInput `shouldBe` Success mempty expect
