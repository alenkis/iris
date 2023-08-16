{-# LANGUAGE OverloadedStrings #-}

module Transform.TransformSpec (spec) where

import           Data.Map.Ordered       as MO
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           QuoteStr               (quoteStr)

import           Config                 (Column (..), Config (..))
import           Data.CSV.Conduit
import           Process                (RowMapIndexed, ValidationResult,
                                         elemIndices', filterWithIndices,
                                         renameHeader, validateField',
                                         validateRow)
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Toml                   (Result (..), decode, encode)
import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue, table, (.=))
import           Toml.ToValue.Generic   (genericToTable)
import           Validation             (Rule (RuleMinLen))

validateRowExpectations :: [([Rule], RowMapIndexed, ValidationResult)]
validateRowExpectations =
    [ ([RuleMinLen 2], (MO.fromList [("field", "h")], 0), Left (["[ field ] Value must be at least 2 characters long. Instead, got: \"h\""], 0))
    , ([RuleMinLen 2], (MO.fromList [("field", "hi")], 0), Right (MO.fromList [("field", "hi")], 0))
    , ([RuleMinLen 2], (MO.fromList [("field", "hi"), ("a", "there")], 0), Right (MO.fromList [("field", "hi"), ("a", "there")], 0))
    ]

validateFieldExpectations :: [((Column, (Text, Text)), Either Text Text)]
validateFieldExpectations =
    [ ((Column "field" Nothing (Just [RuleMinLen 2]), ("field", "h")), Left "[ field ] Value must be at least 2 characters long. Instead, got: \"h\"")
    , ((Column "field" Nothing (Just [RuleMinLen 2]), ("field", "hi")), Right "hi")
    , ((Column "field" Nothing (Just [RuleMinLen 2]), ("field", "hi there")), Right "hi there")
    ]

filterWithIndicesExpectations :: [([Int], [Text], [Text])]
filterWithIndicesExpectations =
    [ ([0], ["h"], ["h"])
    , ([0], ["hi"], ["hi"])
    , ([0], ["hi", "there"], ["hi"])
    , ([1], ["hi", "there"], ["there"])
    , ([0, 1], ["hi", "there"], ["hi", "there"])
    ]

elemIndicesExp :: [([Column], [Text], [Int])]
elemIndicesExp =
    [
        (
            [ Column "field1" Nothing Nothing
            , Column "field2" Nothing Nothing
            , Column "field3" Nothing Nothing
            ]
        , ["field2", "field3"]
        , [0, 1]
        )
    ]

spec :: Spec
spec =
    describe "Transform" $ do
        describe "validateRow" $ do
            mapM_
                ( \(rules, row, expected) ->
                    it ("should satisfy expectations for " ++ show row) $
                        validateRow [Column "field" Nothing (Just rules)] row `shouldBe` expected
                )
                validateRowExpectations
        describe "validateField" $ do
            mapM_
                ( \((field, value), expected) ->
                    it
                        ("should satisfy expectations for validate field " ++ show field)
                        $ validateField' field value `shouldBe` expected
                )
                validateFieldExpectations

        describe "filterWithIndices" $ do
            mapM_
                ( \(indices, row, expected) ->
                    it ("should satisfy expectations for " ++ show row) $
                        filterWithIndices indices row `shouldBe` expected
                )
                filterWithIndicesExpectations

        describe "elemIndices" $
            mapM_
                ( \(fields, row, expected) ->
                    it ("should satisfy expectations for " ++ show row) $
                        elemIndices' fields row `shouldBe` expected
                )
                elemIndicesExp
        describe "renameHeader" $
            it "should rename the header" $
                let
                    fields = [Column "field name" (Just "field rename") Nothing]
                    config =
                        Config
                            { jobTitle = "job title"
                            , jobGroupBy = Nothing
                            , jobSeparator = Just ','
                            , jobColumns = fields
                            }
                 in
                    renameHeader
                        config
                        (MO.singleton ("field name", "original"))
                        `shouldBe` MO.singleton ("field rename", "original")
