module Main where

import           Cli (Command (TransformCSV), getCliCommand)
import           Csv (reverseCsv)

main :: IO ()
main = do
    (TransformCSV configPath filepath) <- getCliCommand
    putStrLn $ "file path: " ++ show filepath
    putStrLn $ "config path: " ++ show configPath
    reverseCsv filepath "output.csv"
