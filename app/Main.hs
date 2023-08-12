{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                  (Command (TransformCSV),
                                       ConfigPath (ConfigPath),
                                       OutputPath (OutputPath), getCliCommand)
import           Config               (Config (..), Job (..), read)
import           Control.Monad.Reader (ReaderT (runReaderT))
import           Data.Text            (unpack)
import qualified Data.Text            as T
import           Transform            (Env (..), transform)

main :: IO ()
main = do
    (TransformCSV (ConfigPath configPath) filepath (OutputPath outputPath)) <- getCliCommand
    Config.read configPath >>= \case
        Left err -> mapM_ print err
        Right config -> do
            _ <- runReaderT (transform (T.pack filepath) (T.pack outputPath)) $ Env config
            let jobName = (jobTitle . job) config
            putStrLn "\n"
            putStrLn $ "Finished job " ++ unpack jobName
            putStrLn $ "Output file: " ++ outputPath
            putStrLn "\n"
            return ()
