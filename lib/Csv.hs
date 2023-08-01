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

filterColumns :: String -> String -> Config -> IO ()
filterColumns sourcePath destinationPath config =
    runResourceT $
        runConduit $
            source
                .| intoCSV settings
                .| processor
                .| fromCSV settings
                .| sink
  where
    fields = (field . job) config
    sep = fromMaybe ',' $ (separator . job) config
    source = sourceFile sourcePath
    settings = csvSettings sep
    processor :: MonadIO m => ConduitT (Row Text) (Row Text) m ()
    processor = do
        mheader <- await
        case mheader of
            Nothing -> return ()
            Just header -> do
                let indices = elemIndices' fields header
                leftover header
                CL.map (filterWithIndices indices)
    sink = sinkFile destinationPath

elemIndices' :: [Field] -> Row Text -> [Int]
elemIndices' fields row =
    concatMap (`elemIndices` row) fs
  where
    fs = map (\(Field n _) -> n) fields

filterWithIndices :: [Int] -> Row Text -> Row Text
filterWithIndices indices row = [row !! i | i <- indices]
