module Config where

import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import qualified Toml
import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue)
import           Toml.ToValue.Generic   (genericToTable)
import           Validation             (Rule, parseValidationRule)

newtype Config = Config {job :: Job}
    deriving (Eq, Show, Generic)

data Job = Job
    { title     :: Text
    , groupBy   :: Text
    , field     :: [Field]
    , separator :: Maybe Char
    }
    deriving (Eq, Show, Generic)

data Field = Field
    { name       :: Text
    , rename     :: Maybe Text
    , validation :: Maybe [Rule]
    }
    deriving (Eq, Show, Generic)

instance FromValue Field where
    fromValue =
        parseTableFromValue $ do
            name <- reqKey "name"
            rename <- optKey "rename"
            validation <- optKey "validation"
            let validations = fmap (mapMaybe parseValidationRule) validation
            return (Field name rename validations)

instance FromValue Config where fromValue = parseTableFromValue genericParseTable
instance FromValue Job where
    fromValue = parseTableFromValue (Job <$> reqKey "title" <*> reqKey "group_by" <*> reqKey "field" <*> optKey "separator")

instance ToValue Config where toValue = defaultTableToValue
instance ToValue Field where toValue = defaultTableToValue
instance ToValue Job where toValue = defaultTableToValue

instance ToTable Field where toTable = genericToTable
instance ToTable Config where toTable = genericToTable
instance ToTable Job where toTable = genericToTable

read :: FilePath -> IO (Either [String] Config)
read path = do
    toml <- readFile path
    case Toml.decode toml of
        Toml.Success _ config -> return (Right config)
        Toml.Failure err      -> return (Left err)
