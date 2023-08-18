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
import           Data.Bifunctor         (bimap)
import           Data.ByteString        (ByteString)
import           Data.CSV.Conduit
import           Data.List              (foldl')
import           Data.Map.Ordered       (assocs)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple

-- | Create a database connection
createConn :: Config -> IO Connection
createConn config = open $ nameDb config
  where
    nameDb :: Config -> String
    nameDb = (<> ".db") . T.unpack . T.intercalate "_" . T.words . T.toLower . jobTitle

-- | Create table schema using CSV columns
createTableQuery :: [Column] -> Query
createTableQuery cols =
    "CREATE TABLE IF NOT EXISTS iris (" <> columnDefinition cols <> ");"
  where
    columnDefinition :: [Column] -> Query
    columnDefinition =
        Query
            . T.intercalate ", "
            -- TODO: try to parse the column type and use appropriate type in the DB
            . fmap (\(Column name _ _) -> name <> " TEXT")

processor :: (Monad m) => ConduitT (OrderedMapRow Text) (OrderedMapRow Text) m ()
processor = do
    maybeRow <- await
    case maybeRow of
        Nothing -> return ()
        Just row -> do
            yield row
            processor

doDiff :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> FilePath -> FilePath -> ReaderT Context m ()
doDiff config f1 f2 = do
    -- Create connection
    conn <- liftIO $ createConn config
    -- Create table
    liftIO $ execute_ conn $ createTableQuery (jobColumns config)
    liftIO $ putStrLn "Created table."
    -- Insert data
    liftIO $ putStrLn "Inserting data..."
    runPipeline config conn f1 f2

insertRow :: (MonadIO m) => Connection -> ConduitT (OrderedMapRow Text) (OrderedMapRow Text) m ()
insertRow conn = do
    maybeRow <- await
    case maybeRow of
        Nothing -> return ()
        Just row -> do
            -- Insert into DB
            _ <- liftIO $ execute_ conn (insertQuery row)
            yield row
            insertRow conn

insertQuery :: OrderedMapRow Text -> Query
insertQuery mapRow =
    Query
        "INSERT INTO iris ("
        <> Query columns
        <> ") VALUES ("
        <> Query values
        <> ")"
  where
    (columns, values) =
        foldl' (\(a1, a2) (k, v) -> (a1 <> k <> ", ", a2 <> v <> ", ")) ("", "") (assocs mapRow)

writeAndConvert :: (Monad m) => CSVSettings -> ConduitT (OrderedMapRow Text) ByteString m ()
writeAndConvert settings = writeHeadersOrdered settings >> fromCSV settings

runPipeline :: (MonadIO m, MonadUnliftIO m, MonadThrow m) => Config -> Connection -> FilePath -> FilePath -> ReaderT Context m ()
runPipeline config conn oldFile _ =
    runResourceT $
        runConduit $
            sourceFile
                oldFile
                .| intoCSV settings
                .| processor
                .| insertRow conn
                .| writeAndConvert settings
                .| sinkNull
  where
    settings = csvSettings (fromMaybe ',' $ jobSeparator config)
    csvSettings :: Char -> CSVSettings
    csvSettings sep = defCSVSettings{csvSep = sep, csvQuoteChar = Nothing}
