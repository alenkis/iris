module Csv where

import           Conduit
import           Data.Conduit.List as CL
import           Data.CSV.Conduit
import           Data.Text         (Text)

-- Just reverse te columns
myProcessor :: Monad m => ConduitT (Row Text) (Row Text) m ()
myProcessor = CL.map reverse

reverseCsv :: String -> String -> IO ()
reverseCsv source destination =
    runResourceT $
        transformCSV
            defCSVSettings
            (sourceFile source)
            myProcessor
            (sinkFile destination)
