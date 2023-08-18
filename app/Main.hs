{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                  (Command (..), ConfigPath (ConfigPath),
                                       OutputPath (OutputPath), getCliCommand)
import           Config               (Config (..), read)
import           Context              (mkContext)
import           Control.Monad.Reader (ReaderT (runReaderT))
import           Data.Text            (unpack)
import qualified Data.Text            as T
import           Db
import           Process              (transform)

main :: IO ()
main = do
    getCliCommand >>= \case
        TransformCSV (ConfigPath configPath) filepath (OutputPath outputPath) ->
            handleTransform configPath filepath outputPath
        DiffCSV (ConfigPath configPath) fileOld fileNew ->
            handleDiff configPath fileOld fileNew

handleTransform :: FilePath -> String -> String -> IO ()
handleTransform configPath filePath outputPath = do
    Config.read configPath >>= \case
        Left err -> mapM_ print err
        Right config -> do
            _ <- runReaderT (transform (T.pack filePath) (T.pack outputPath)) $ mkContext config
            putStrLn $ "\nFinished job " ++ unpack (jobTitle config)
            putStrLn $ "Output file: " ++ outputPath ++ "\n"
            return ()

handleDiff :: FilePath -> String -> String -> IO ()
handleDiff configPath f1 f2 = do
    Config.read configPath >>= \case
        Left err -> mapM_ print err
        Right config -> do
            result <- runReaderT (doDiff config f1 f2) (mkContext config)
            putStrLn "\nFinished diffing"
            print result
