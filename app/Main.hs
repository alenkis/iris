{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli       (Command (TransformCSV), ConfigPath (ConfigPath),
                            OutputPath (OutputPath), getCliCommand)
import           Config    (Config (..), Job (..), read)
import qualified Csv
import           Data.Text (unpack)

main :: IO ()
main = do
    (TransformCSV (ConfigPath configPath) filepath (OutputPath outputPath)) <- getCliCommand
    Config.read configPath >>= \case
        Left err -> mapM_ putStrLn err
        Right config -> do
            Csv.process config filepath outputPath
            let jobName = (title . job) config
            putStrLn $ "Finished job " ++ unpack jobName
            putStrLn $ "Output file: " ++ outputPath
            return ()
