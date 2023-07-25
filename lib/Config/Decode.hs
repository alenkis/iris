module Config.Decode where

-- import           GHC.Generics (Generic)

newtype Config = Config {port :: Int} deriving (Show, Eq)
