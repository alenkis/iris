module Config.Decode where

import           GHC.Generics           (Generic)

import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue)
import           Toml.ToValue.Generic   (genericToTable)

data Job = Job
    { title   :: String
    , groupBy :: String
    , field   :: [Field]
    }
    deriving (Eq, Show, Generic)

newtype Config = Config {job :: Job}
    deriving (Eq, Show, Generic)

data Field = Field
    { name   :: String
    , rename :: Maybe String
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
