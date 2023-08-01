module Config where

import           GHC.Generics           (Generic)

import           Data.Text              (Text)
import qualified Toml
import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue)
import           Toml.ToValue.Generic   (genericToTable)

newtype Config = Config {job :: Job}
    deriving (Eq, Show, Generic)

data Job = Job
    { title   :: Text
    , groupBy :: Text
    , field   :: [Field]
    }
    deriving (Eq, Show, Generic)

data Field = Field
    { name   :: Text
    , rename :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance FromValue Field where
    fromValue = parseTableFromValue (Field <$> reqKey "name" <*> optKey "rename")

instance FromValue Config where fromValue = parseTableFromValue genericParseTable
instance FromValue Job where
    fromValue = parseTableFromValue (Job <$> reqKey "title" <*> reqKey "group_by" <*> reqKey "field")

instance ToValue Config where toValue = defaultTableToValue
instance ToValue Field where toValue = defaultTableToValue
instance ToValue Job where toValue = defaultTableToValue

instance ToTable Field where toTable = genericToTable
instance ToTable Config where toTable = genericToTable
instance ToTable Job where toTable = genericToTable

readConfig :: FilePath -> IO (Either [String] Config)
readConfig path = do
    toml <- readFile path
    case Toml.decode toml of
        Toml.Success _ config -> return (Right config)
        Toml.Failure err      -> return (Left err)
