{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                  (Command (..), ConfigPath (ConfigPath),
                                       OutputPath (OutputPath), getCliCommand)
import           Config               (Config (..), read)
import           Control.Monad.Reader (ReaderT (runReaderT))
import           Data.Text            (unpack)
import qualified Data.Text            as T
import           Process              (Env (..), transform)

main :: IO ()
main =
    getCliCommand >>= \case
        TransformCSV (ConfigPath configPath) filepath (OutputPath outputPath) ->
            handleTransform configPath filepath outputPath
        DiffCSV a b -> handleDiff a b

handleTransform :: FilePath -> String -> String -> IO ()
handleTransform configPath filePath outputPath =
    Config.read configPath >>= \case
        Left err -> mapM_ print err
        Right config -> do
            _ <- runReaderT (transform (T.pack filePath) (T.pack outputPath)) $ Env config
            putStrLn $ "\nFinished job " ++ unpack (jobTitle config)
            putStrLn $ "Output file: " ++ outputPath ++ "\n"
            return ()

handleDiff :: FilePath -> FilePath -> IO ()
handleDiff _ _ = putStrLn "Diff command not implemented yet"
