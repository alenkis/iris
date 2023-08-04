module Transform where

import           Conduit
import qualified Conduit              as CL
import           Config               (Config (..), Field (..), Job (..))
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import qualified Data.Conduit.List    as CL
import           Data.CSV.Conduit
import           Data.Either          (isRight, lefts)
import           Data.List            (elemIndices)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Validation           (validateField)

class (Monad m) => CsvM m where
    transform :: String -> String -> m ()

newtype Env = Env {envConfig :: Config}

instance (MonadIO m, MonadUnliftIO m, MonadThrow m) => CsvM (ReaderT Env m) where
    transform source destination = do
        envConfig <- asks envConfig
        process' envConfig source destination

class HasConfig env where
    getConfig :: env -> Config

instance HasConfig Env where
    getConfig = envConfig

csvSettings :: Char -> CSVSettings
csvSettings sep = defCSVSettings{csvSep = sep, csvQuoteChar = Nothing}

renameHeader :: Config -> Row Text -> Row Text
renameHeader config = map renameField
  where
    fields = (field . job) config
    renameField value = fromMaybe value $ lookup value renameMapping
    renameMapping = [(name, fromMaybe name rename) | Field name rename _ <- fields]

process' :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> String -> String -> ReaderT Env m ()
process' config sourcePath destinationPath =
    runResourceT $
        runConduit $
            source
                .| intoCSV settings
                .| processor fields
                .| fromCSV settings
                .| sink
  where
    fields = (field . job) config
    sep = fromMaybe ',' $ (separator . job) config
    source = sourceFile sourcePath
    settings = csvSettings sep
    sink = sinkFile destinationPath

validateRow :: [Field] -> Row Text -> Either [Text] (Row Text)
validateRow fields row =
    case traverse validateField' (zip fields row) of
        Left errors -> Left [errors]
        Right validFields -> Right $ filterWithIndices (elemIndices' fields row) validFields

validateField' :: (Field, Text) -> Either Text Text
validateField' (field, value) =
    let rules = fromMaybe [] $ validation field
        results = map (\rule -> validateField rule value) rules
        allValid = all isRight results
        ls = lefts results
     in if allValid
            then Right value
            else Left $ T.unwords ls

processor :: (Monad m, HasConfig env, MonadReader env m) => [Field] -> ConduitT (Row Text) (Row Text) m ()
processor fields = do
    config <- asks getConfig
    row <- await
    case row of
        Nothing -> return ()
        Just header -> do
            let renamedHeader = renameHeader config header
            leftover renamedHeader
            CL.mapM processRow
  where
    processRow row = case validateRow fields row of
        Left errors -> do
            return errors
        Right validRow -> return validRow

elemIndices' :: [Field] -> Row Text -> [Int]
elemIndices' fields row =
    concatMap (`elemIndices` row) fs
  where
    fs :: [Text]
    fs = map (\(Field n _ _) -> n) fields

filterWithIndices :: [Int] -> Row Text -> Row Text
filterWithIndices indices row = [row !! i | i <- indices]
