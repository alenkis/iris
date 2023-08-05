{-# LANGUAGE OverloadedStrings #-}

module Transform.TransformSpec (spec) where

import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           QuoteStr               (quoteStr)

import           Config                 (Config (..), Field (..), Job (..))
import           Data.CSV.Conduit
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Toml                   (Result (..), decode, encode)
import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue, table, (.=))
import           Toml.ToValue.Generic   (genericToTable)
import           Transform              (elemIndices', filterWithIndices,
                                         validateField', validateRow)
import           Validation             (Rule (RuleMinLen))

validateRowExpectations :: [([Rule], [Text], Either [Text] [Text])]
validateRowExpectations =
    [ ([RuleMinLen 2], ["h"], Left ["Value must be at least 2 characters long"])
    , ([RuleMinLen 2], ["hi"], Right ["hi"])
    , ([RuleMinLen 2], ["hi", "there"], Right ["hi", "there"])
    ]

validateFieldExpectations :: [((Field, Text), Either Text Text)]
validateFieldExpectations =
    [ ((Field "field" Nothing (Just [RuleMinLen 2]), "h"), Left "Value must be at least 2 characters long")
    , ((Field "field" Nothing (Just [RuleMinLen 2]), "hi"), Right "hi")
    , ((Field "field" Nothing (Just [RuleMinLen 2]), "hi there"), Right "hi there")
    ]

filterWithIndicesExpectations :: [([Int], [Text], [Text])]
filterWithIndicesExpectations =
    [ ([0], ["h"], ["h"])
    , ([0], ["hi"], ["hi"])
    , ([0], ["hi", "there"], ["hi"])
    , ([1], ["hi", "there"], ["there"])
    , ([0, 1], ["hi", "there"], ["hi", "there"])
    ]

elemIndicesExp :: [([Field], [Text], [Int])]
elemIndicesExp =
    [
        (
            [ Field "field1" Nothing Nothing
            , Field "field2" Nothing Nothing
            , Field "field3" Nothing Nothing
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
                        validateRow [Field "field" Nothing (Just rules)] row `shouldBe` expected
                )
                validateRowExpectations
        describe "validateField" $ do
            mapM_
                ( \(field, expected) ->
                    it ("should satisfy expectations for validate field " ++ show field) $
                        validateField' field `shouldBe` expected
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
