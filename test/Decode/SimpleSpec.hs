module Decode.SimpleSpec (spec) where

import           Data.Maybe             (fromMaybe)
import           GHC.Generics           (Generic)
import           QuoteStr               (quoteStr)

import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Toml                   (Result (..), decode, encode)
import           Toml.FromValue         (FromValue (..), optKey,
                                         parseTableFromValue, reqKey)
import           Toml.FromValue.Generic (genericParseTable)
import           Toml.ToValue           (ToTable (..), ToValue (toValue),
                                         defaultTableToValue, table, (.=))
import           Toml.ToValue.Generic   (genericToTable)

data Job = Job
    { title  :: String
    , fields :: [Field]
    }
    deriving (Eq, Show, Generic)

newtype Config = Config
    { job :: Job
    }
    deriving (Eq, Show, Generic)

data Field = Field
    { name   :: String
    , rename :: String
    }
    deriving (Eq, Show, Generic)

instance FromValue Field where fromValue = parseTableFromValue genericParseTable
instance FromValue Config where fromValue = parseTableFromValue genericParseTable
instance FromValue Job where fromValue = parseTableFromValue genericParseTable

instance ToValue Config where toValue = defaultTableToValue
instance ToValue Field where toValue = defaultTableToValue
instance ToValue Job where toValue = defaultTableToValue

instance ToTable Field where toTable = genericToTable
instance ToTable Config where toTable = genericToTable
instance ToTable Job where toTable = genericToTable

spec :: Spec
spec =
    do
        let expect =
                Config
                    { job =
                        Job
                            { title = "TOML Example"
                            , fields =
                                [ Field
                                    { name = "item_group_id"
                                    , rename = "group_id"
                                    }
                                , Field
                                    { name = "name"
                                    , rename = "name"
                                    }
                                ]
                            }
                    }

        let input =
                [quoteStr|
        [job]
        title = "TOML Example"

        [[ job.fields ]]
        name = "item_group_id"
        rename = "group_id"

        [[ job.fields ]]
        name = "name"
        rename = "name"
        |]

        describe "simple config" $
            do
                it "should fail" $ do
                    decode input `shouldBe` Success mempty expect
