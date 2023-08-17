module Context where

import           Config (Config)

newtype Context = Context {unContext :: Context_}
newtype Context_ = Context_ {config :: Config}

class HasConfig ctx where
    getConfig :: ctx -> Config

instance HasConfig Context where
    getConfig = config . unContext

mkContext :: Config -> Context
mkContext = Context . Context_
