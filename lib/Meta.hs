module Meta (appVersion) where

import           Data.Version (showVersion)
import           Paths_iris   (version)

appVersion :: String
appVersion = showVersion version
