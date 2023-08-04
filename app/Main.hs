{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                  (Command (TransformCSV),
                                       ConfigPath (ConfigPath),
                                       OutputPath (OutputPath), getCliCommand)
import           Config               (Config (..), Job (..), read)
import           Control.Monad.Reader (ReaderT (runReaderT))
import           Data.Text            (unpack)
import           Transform            (Env (..), transform)

main :: IO ()
main = do
    (TransformCSV (ConfigPath configPath) filepath (OutputPath outputPath)) <- getCliCommand
    Config.read configPath >>= \case
        Left err -> mapM_ putStrLn err
        Right config -> do
            let env = Env{envConfig = config}
            _ <- runReaderT (transform filepath outputPath) env
            let jobName = (title . job) config
            putStrLn $ "Finished job " ++ unpack jobName
            putStrLn $ "Output file: " ++ outputPath
            print config
            return ()
