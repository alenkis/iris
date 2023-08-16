{-# LANGUAGE OverloadedStrings #-}

module Config.SimpleSpec (spec) where

import           Data.Maybe             (fromMaybe)
import           GHC.Generics           (Generic)
import           QuoteStr               (quoteStr)

import           Config                 (Column (..), Config (..))
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
        let expect =
                Config
                    { jobTitle = "TOML Example"
                    , jobGroupBy = Just "item_group_id"
                    , jobSeparator = Just ','
                    , jobColumns =
                        [ Column
                            { columnName = "item_group_id"
                            , columnRename = Just "group_id"
                            , columnValidationRules = Nothing
                            }
                        , Column
                            { columnName = "name"
                            , columnRename = Nothing
                            , columnValidationRules = Nothing
                            }
                        ]
                    }

        let decodedInput =
                decode
                    [quoteStr|
        title = "TOML Example"
        group_by = "item_group_id"
        separator = ","

        [[columns]]
        name = "item_group_id"
        rename = "group_id"

        [[columns]]
        name = "name"
        |]

        describe "simple config" $ do
            it "should decode successfully" $ do
                decodedInput `shouldBe` Success mempty expect
