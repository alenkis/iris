{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli    (Command (TransformCSV), ConfigPath (ConfigPath),
                         getCliCommand)
import           Config (Config (..), Job (..), read)
import qualified Csv

main :: IO ()
main = do
    (TransformCSV (ConfigPath configPath) filepath) <- getCliCommand
    Config.read configPath >>= \case
        Left err -> mapM_ putStrLn err
        Right cfg -> Csv.filterColumns filepath "./examples/output.csv" $ (field . job) cfg
    putStrLn "Done."
