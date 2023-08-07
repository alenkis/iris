{-# LANGUAGE OverloadedStrings #-}

module Validation where

import           Data.Bifunctor  (second)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))
import           Toml.ToValue    (ToValue (toValue))
import qualified Toml.Value      as TomlV

type ColumnName = Text

type Choice = Text

data Rule
    = RuleNonEmpty
    | RuleMinLen Int
    | RuleMaxLen Int
    | RuleOneOf [Choice]
    | RuleRegex Text
    deriving (Eq, Show)

instance ToValue Rule where
    toValue (RuleMinLen min') = TomlV.String $ "min_length:" <> show min'
    toValue (RuleMaxLen max') = TomlV.String $ "max_length:" <> show max'
    toValue (RuleOneOf choices) = TomlV.String $ "one_of:" <> T.unpack (T.intercalate "," choices)
    toValue (RuleRegex r) = TomlV.String $ "regex:" <> T.unpack r
    toValue RuleNonEmpty = TomlV.String "non_empty"

parseValidationRule :: Text -> Maybe Rule
parseValidationRule ruleText =
    let (ruleName, args) =
            second
                (T.drop 1) -- Remove the leading colon
                (T.breakOn ":" ruleText)
     in case ruleName of
            "non_empty" -> Just RuleNonEmpty
            "min_length" -> fmap RuleMinLen $ readMaybe $ T.unpack args
            "max_length" -> fmap RuleMaxLen $ readMaybe $ T.unpack args
            "one_of" -> if T.null args then Nothing else Just . RuleOneOf $ T.splitOn "," args
            "regex" -> if T.null args then Nothing else Just $ RuleRegex args
            _ -> Nothing

validateField :: Rule -> Text -> Either Text ()
validateField rule value =
    case rule of
        RuleNonEmpty -> if T.null value then Left "Value must not be empty" else Right ()
        RuleMinLen min' ->
            if T.length value < min'
                then Left (T.pack $ "Value must be at least " ++ show min' ++ " characters long. Instead, got: " ++ show value)
                else Right ()
        RuleMaxLen max' ->
            if T.length value > max'
                then Left (T.pack $ "Value must be at most " ++ show max' ++ " characters long")
                else Right ()
        RuleOneOf choices ->
            if value `notElem` choices
                then Left (T.pack $ "Value must be one of " ++ show choices)
                else Right ()
        RuleRegex pattern ->
            if value =~ T.unpack pattern
                then Right ()
                else Left (T.pack $ "Value must match regex " ++ T.unpack pattern)
