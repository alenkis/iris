module Csv where

import           Conduit
import           Config            (Config (..), Field (..), Job (..))
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit
import           Data.List         (elemIndices)
import           Data.Maybe        (fromMaybe)
import           Data.Text         (Text)

csvSettings :: Char -> CSVSettings
csvSettings sep = defCSVSettings{csvSep = sep, csvQuoteChar = Nothing}

renameHeader :: Config -> Row Text -> Row Text
renameHeader config = map renameField
  where
    fields = (field . job) config
    renameField value = fromMaybe value $ lookup value renameMapping
    renameMapping = [(name, fromMaybe name rename) | Field name rename <- fields]

processor :: Monad m => Config -> [Field] -> ConduitT (Row Text) (Row Text) m ()
processor config fields = do
    row <- await
    case row of
        Nothing -> return ()
        Just header -> do
            let indices = elemIndices' fields header
            let renamedHeader = renameHeader config header
            leftover renamedHeader
            CL.map (filterWithIndices indices)

process :: Config -> String -> String -> IO ()
process config sourcePath destinationPath =
    runResourceT $
        runConduit $
            source
                .| intoCSV settings
                .| processor config fields
                .| fromCSV settings
                .| sink
  where
    fields = (field . job) config
    sep = fromMaybe ',' $ (separator . job) config
    source = sourceFile sourcePath
    settings = csvSettings sep
    sink = sinkFile destinationPath

elemIndices' :: [Field] -> Row Text -> [Int]
elemIndices' fields row =
    concatMap (`elemIndices` row) fs
  where
    fs :: [Text]
    fs = map (\(Field n _) -> n) fields

filterWithIndices :: [Int] -> Row Text -> Row Text
filterWithIndices indices row = [row !! i | i <- indices]
