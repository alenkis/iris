{-# LANGUAGE OverloadedStrings #-}

{- | SQLITE
| 0. Create table
| 1. Create table schema using CSV columns
| 2. Insert CSV data into table
|
| Diffing:
|  3. Repeat for second file
|  4. Diff tables
|
| Grouping:
|  3. ...
-}
module Db where

import           Conduit                as CL
import           Config                 (Column (..), Config (..), jobColumns)
import           Context                (Context)
import           Control.Monad.Reader   (ReaderT)
import           Data.CSV.Conduit
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Text              as T
import           Database.SQLite.Simple
import           Process                (csvSettings)

diffFiles :: Config -> FilePath -> FilePath -> IO ()
diffFiles config oldFile newFile = do
    let columns = jobColumns config
    conn <- open "test.db"
    let tableQ = createTable columns
    print tableQ
    execute_ conn tableQ
    print (oldFile, newFile)
    return ()

--

-- | Create a database connection
createConn :: Config -> IO Connection
createConn config = open $ nameDb config

-- | Create table schema using CSV columns
columnDefinition :: [Column] -> Query
columnDefinition =
    Query
        . T.intercalate ", "
        . fmap (\(Column name _ _) -> name <> " TEXT")

createTable cols = "CREATE TABLE IF NOT EXISTS iris (" <> columnDefinition cols <> ");"

nameDb :: Config -> String
nameDb = undefined

pipeline :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> FilePath -> FilePath -> ReaderT Context m ()
pipeline config oldFile _ = do
    let
        settings = csvSettings (fromMaybe ',' $ jobSeparator config)
        p =
            sourceFile oldFile
                .| intoCSV settings
                .| processor
                .| (writeHeadersOrdered settings >> fromCSV settings)
                .| sinkFile "foobar.txt"
     in
        runResourceT $
            runConduit p

processor :: (Monad m) => ConduitT (OrderedMapRow Text) (OrderedMapRow Text) m ()
processor = do
    maybeRow <- await
    case maybeRow of
        Nothing -> return ()
        Just row -> do
            yield row
            processor

--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
