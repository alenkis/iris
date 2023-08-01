module Csv where

import           Conduit
import           Config            (Field (..))
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit
import           Data.List         (elemIndices)
import           Data.Text         (Text)

csvSettings :: Char -> CSVSettings
csvSettings sep = defCSVSettings{csvSep = sep, csvQuoteChar = Nothing}

filterColumns :: String -> String -> [Field] -> IO ()
filterColumns sourcePath destinationPath fields =
    runResourceT $
        runConduit $
            source
                .| intoCSV settings
                .| processor
                .| fromCSV settings
                .| sink
  where
    source = sourceFile sourcePath
    settings = csvSettings '\t'
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
    let fs = map (\(Field n _) -> n) fields
     in concatMap (`elemIndices` row) fs

filterWithIndices :: [Int] -> Row Text -> Row Text
filterWithIndices indices row = [row !! i | i <- indices]
