module Transform where

import           Conduit              as CL
import qualified Data.Map.Strict      as M

import           Config               (Config (..), Field (..), Job (..))
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Data.Bifunctor       (first)
import           Data.CSV.Conduit
import           Data.Either          (isRight, lefts)
import           Data.List            (elemIndices)
import qualified Data.Map.Ordered     as MO
import           Data.Maybe           (catMaybes, fromMaybe, mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Debug.Trace          (traceShow)
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

renameHeader :: Config -> OrderedMapRow Text -> OrderedMapRow Text
renameHeader config oldmaprow =
    MO.fromList $
        mapMaybe (filterVals fields . first renameField) (MO.assocs oldmaprow)
  where
    fields = (jobField . job) config
    renameField value = fromMaybe value $ lookup value renameMapping
    renameMapping = [(name, fromMaybe name rename) | Field name rename _ <- fields]

filterVals :: [Field] -> (Text, Text) -> Maybe (Text, Text)
filterVals fields (k, v) =
    if k `elem` map fieldName fields || k `elem` mapMaybe fieldRename fields
        then Just (k, v)
        else Nothing

process' :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> String -> String -> ReaderT Env m ()
process' config sourcePath destinationPath =
    runResourceT $
        runConduit $
            source
                -- create a stream of CSV Rows
                .| intoCSV settings
                -- validate the rows
                .| validateConduit fields
                -- log invalid rows, and pass valid ones
                .| reportInvalid
                -- filter out columns
                .| filterValues fields
                -- process (transform) the rows
                .| processor fields
                -- write headers and create a bytestream from CSV Rows
                .| (writeHeadersOrdered settings >> fromCSV settings)
                -- output
                .| sink
  where
    fields = (jobField . job) config
    sep = fromMaybe ',' $ (jobSeparator . job) config
    source = sourceFile sourcePath
    settings = csvSettings sep
    sink = sinkFile destinationPath

filterValues :: (Monad m) => [Field] -> ConduitT (OrderedMapRow Text) (OrderedMapRow Text) m ()
filterValues fields = do
    row <- await
    case row of
        Nothing -> return ()
        Just maprow -> do
            yield $
                MO.fromList $
                    mapMaybe (filterVals fields) (MO.assocs maprow)
            filterValues fields

{- | For every `Left` value in the stream, collects the error and console logs it.
| For every `Right` value in the stream, passes it on.
-}
reportInvalid :: (MonadIO m) => ConduitT (Either [Text] (OrderedMapRow Text)) (OrderedMapRow Text) m ()
reportInvalid = do
    row <- await
    case row of
        Nothing -> return ()
        Just r -> do
            case r of
                Left errors -> liftIO $ putStrLn $ T.unpack $ T.unwords errors
                Right a     -> yield a
            reportInvalid

-- | Validates a conduit row and outputs the result of the validation as an `Either`.
validateConduit :: (Monad m) => [Field] -> ConduitT (OrderedMapRow Text) (Either [Text] (OrderedMapRow Text)) m ()
validateConduit fields = do
    row <- await
    case row of
        Nothing -> return ()
        Just r -> do
            yield $ validateRow fields r
            validateConduit fields

validateRow :: [Field] -> OrderedMapRow Text -> Either [Text] (OrderedMapRow Text)
validateRow fields maprow =
    let
        results = zipWith validateField' fields (MO.assocs maprow)
        errors = lefts results
     in
        if null errors
            then Right maprow
            else Left errors

validateField' :: Field -> (Text, Text) -> Either Text Text
validateField' field (columnName, value) =
    let rules = fromMaybe [] $ fieldValidation field
        doesNameMatch = fieldName field == columnName || fieldRename field == Just columnName
        results =
            -- traceShow (field, rules) $
            map (`validateField` (columnName, value)) rules
        allValid =
            -- traceShow (fieldName field, value, results) $
            all isRight results
        ls = T.unwords $ lefts results
     in if allValid
            then Right value
            else Left ls

-- |  Processes text rows.
processor :: (Monad m, HasConfig env, MonadReader env m) => [Field] -> ConduitT (OrderedMapRow Text) (OrderedMapRow Text) m ()
processor fields = do
    config <- asks getConfig
    row <- await
    case row of
        Nothing -> return ()
        Just r -> do
            -- Rename headers
            leftover $ renameHeader config r
            -- CL.mapM processRow
            CL.filterC
                ( \maprow -> case validateRow fields maprow of
                    Left _  -> False
                    Right _ -> True
                )

elemIndices' :: [Field] -> Row Text -> [Int]
elemIndices' fields row =
    let fs :: [Text]
        fs = map fieldName fields
        indices = concatMap (`elemIndices` row) fs
     in if null indices then [0 .. (length row - 1)] else indices

filterWithIndices :: [Int] -> Row Text -> Row Text
filterWithIndices indices row = [row !! i | i <- indices, i < length row]
