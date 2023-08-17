{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Text            as T

import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import qualified Toml
import           Toml.FromValue       (FromValue (..), optKey,
                                       parseTableFromValue, reqKey)
import           Toml.ToValue         (ToTable (..), ToValue (toValue),
                                       defaultTableToValue)
import           Toml.ToValue.Generic (genericToTable)
import           Validation           (Rule, parseValidationRule)

data Config = Config
    { jobTitle     :: Text
    , jobGroupBy   :: Maybe Text
    , jobSeparator :: Maybe Char
    , jobColumns   :: [Column]
    }
    deriving (Eq, Show, Generic)

data Column = Column
    { columnName            :: Text
    , columnRename          :: Maybe Text
    , columnValidationRules :: Maybe [Rule]
    }
    deriving (Eq, Show, Generic)

instance FromValue Column where
    fromValue =
        parseTableFromValue $ do
            name <- reqKey "name"
            rename <- optKey "rename"
            validation <- optKey "validation"
            let validations = fmap (mapMaybe parseValidationRule) validation
            return (Column name rename validations)

instance FromValue Config where
    fromValue =
        parseTableFromValue
            ( Config
                <$> reqKey "title"
                <*> optKey "group_by"
                <*> optKey "separator"
                <*> reqKey "columns"
            )

instance ToValue Config where toValue = defaultTableToValue
instance ToValue Column where toValue = defaultTableToValue

instance ToTable Column where toTable = genericToTable
instance ToTable Config where toTable = genericToTable

read :: FilePath -> IO (Either [Text] Config)
read path = do
    toml <- readFile path
    case Toml.decode toml of
        Toml.Success _ config -> return (Right config)
        Toml.Failure err      -> return (Left $ map T.pack (err ++ [toml]))
