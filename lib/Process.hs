module Process where

import           Conduit              as CL
import           Config               (Column (..), Config (..))
import           Context              (Context (unContext), Context_ (config),
                                       HasConfig (..))
import           Control.Monad        (unless)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.State  (MonadState (get, put), StateT,
                                       evalStateT)
import           Data.Bifunctor       (first)
import           Data.CSV.Conduit
import           Data.Either          (isRight, lefts)
import           Data.List            (elemIndices, (\\))
import qualified Data.Map.Ordered     as MO
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Validation           (validateField)

type RowMap = OrderedMapRow Text
type RowMapIndexed = (RowMap, Int)
type ErrorsIndexed = (Row Text, Int)
type ValidationResult = Either ErrorsIndexed RowMapIndexed

class (Monad m) => Processor m where
    transform :: Text -> Text -> m ()
    diff :: Text -> Text -> m ()

instance (MonadIO m, MonadUnliftIO m, MonadThrow m) => Processor (ReaderT Context m) where
    transform source destination = do
        config <- asks (config . unContext)
        doTransform config source destination
    diff source destination = do
        config <- asks (config . unContext)
        doDiff config source destination

csvSettings :: Char -> CSVSettings
csvSettings sep = defCSVSettings{csvSep = sep, csvQuoteChar = Nothing}

renameHeader :: Config -> OrderedMapRow Text -> OrderedMapRow Text
renameHeader config =
    mapKeysM (filterVals fields . first renameField)
  where
    fields = jobColumns config
    renameField value = fromMaybe value $ lookup value renameMapping
    renameMapping = [(name, fromMaybe name rename) | Column name rename _ <- fields]

filterVals :: [Column] -> (Text, Text) -> Maybe (Text, Text)
filterVals fields (k, v) =
    if k `elem` map columnName fields || k `elem` mapMaybe columnRename fields
        then Just (k, v)
        else Nothing

numberRows :: (Monad m) => ConduitT a (a, Int) (StateT Int m) ()
numberRows = do
    maybeRow <- await
    case maybeRow of
        Nothing -> return ()
        Just row -> do
            rowNum <- lift get
            yield (row, rowNum)
            lift $ put (rowNum + 1)
            numberRows

doTransform :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> Text -> Text -> ReaderT Context m ()
doTransform config sourcePath destinationPath =
    runResourceT $
        evalStateT (runConduit pipeline) 2 -- start at 2 because of headers
  where
    fields = jobColumns config
    sep = fromMaybe ',' $ jobSeparator config
    source = sourceFile $ T.unpack sourcePath
    settings = csvSettings sep
    pipeline =
        source
            -- create a stream of CSV Rows
            .| intoCSV settings
            -- increment row
            .| numberRows
            -- validate the rows
            .| validateConduit fields
            -- log invalid rows, and pass valid ones
            .| reportInvalid "./errors.txt"
            -- filter out columns
            .| filterValues fields
            -- process (transform) the rows
            .| processor fields
            -- write headers and create a bytestream from CSV Rows
            .| (writeHeadersOrdered settings >> fromCSV settings)
            -- output
            .| sinkFile (T.unpack destinationPath)

doDiff :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> Text -> Text -> ReaderT Context m ()
doDiff config fileOld fileNew =
    runResourceT $ runConduit pipeline
  where
    settings = csvSettings (fromMaybe ',' $ jobSeparator config)
    pipeline = undefined

    diffRows :: ([String], [String]) -> [[String]]
    diffRows (old, new)
        | old == new = ["same" : old]
        | otherwise = ["changed" : old] ++ ["added" : (new \\ old)] ++ ["removed" : (old \\ new)]

filterValues :: (Monad m) => [Column] -> ConduitT RowMapIndexed RowMapIndexed m ()
filterValues fields = do
    row <- await
    case row of
        Nothing -> return ()
        Just (maprow, i) -> do
            yield (mapKeysM (filterVals fields) maprow, i)
            filterValues fields

mapKeysM :: ((Text, Text) -> Maybe (Text, Text)) -> OrderedMapRow Text -> OrderedMapRow Text
mapKeysM f = MO.fromList . mapMaybe f . MO.assocs

batchSize :: Int
batchSize = 100

{- | For every `Left` value in the stream, collects the error and console logs it.
| For every `Right` value in the stream, passes it on.
-}
reportInvalid :: (MonadIO m) => FilePath -> ConduitT ValidationResult RowMapIndexed m ()
reportInvalid errorFilePath = loop []
  where
    loop errorBuffer = do
        row <- await
        case row of
            Nothing -> do
                -- At the end, write any remaining errors.
                unless (null errorBuffer) $ liftIO $ TIO.appendFile errorFilePath (T.unlines errorBuffer)
            Just (Left (errors, idx :: Int)) -> do
                let errorMsg = T.concat [T.pack "Error at row ", T.pack (show idx), T.pack ": ", T.unwords errors]
                let newBuffer = errorBuffer ++ [errorMsg]
                if length newBuffer >= batchSize
                    then do
                        liftIO $ TIO.appendFile errorFilePath (T.unlines newBuffer)
                        loop []
                    else loop newBuffer
            Just (Right a) -> do
                yield a
                loop errorBuffer

-- | Validates a conduit row and outputs the result of the validation as an `Either`.
validateConduit :: (Monad m) => [Column] -> ConduitT RowMapIndexed ValidationResult m ()
validateConduit fields = do
    row <- await
    case row of
        Nothing -> return ()
        Just r -> do
            yield $ validateRow fields r
            validateConduit fields

validateRow :: [Column] -> RowMapIndexed -> ValidationResult
validateRow fields (maprow, index) =
    let
        results = zipWith validateField' fields (MO.assocs maprow)
        errors = lefts results
     in
        if null errors
            then Right (maprow, index)
            else Left (errors, index)

validateField' :: Column -> (Text, Text) -> Either Text Text
validateField' field (columnName, value) =
    let rules = fromMaybe [] $ columnValidationRules field
        results = map (`validateField` (columnName, value)) rules
        allValid = all isRight results
        ls = T.unwords $ lefts results
     in if allValid
            then Right value
            else Left ls

-- |  Processes text rows.
processor :: (Monad m, HasConfig env, MonadReader env m) => [Column] -> ConduitT RowMapIndexed RowMap m ()
processor fields = do
    config <- asks getConfig
    row <- await
    case row of
        Nothing -> return ()
        Just (r, _) ->
            do
                yield $ renameHeader config r
                foobar
  where
    foobar :: (Monad m) => ConduitT RowMapIndexed RowMap m ()
    foobar = CL.filterC (isRight . validateRow fields) .| CL.mapC fst

elemIndices' :: [Column] -> Row Text -> [Int]
elemIndices' fields row =
    let
        indices :: [Int]
        indices = concatMap ((`elemIndices` row) . columnName) fields
     in
        if null indices
            then [0 .. (length row - 1)]
            else indices

filterWithIndices :: [Int] -> Row Text -> Row Text
filterWithIndices indices row = [row !! i | i <- indices, i < length row]
