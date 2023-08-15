module Cli where

import           Meta                (appVersion)
import           Options.Applicative (CommandFields, Mod, Parser, argument,
                                      command, execParser, fullDesc, header,
                                      help, helper, info, infoOption, long,
                                      metavar, progDesc, short, str, strOption,
                                      subparser)

newtype ConfigPath = ConfigPath FilePath
    deriving (Eq, Show)

newtype OutputPath = OutputPath FilePath
    deriving (Eq, Show)

data Command
    = TransformCSV ConfigPath FilePath OutputPath
    | DiffCSV FilePath FilePath
    deriving (Eq, Show)

configPathP :: Parser ConfigPath
configPathP = ConfigPath <$> strOption configField
  where
    configField =
        short 'c'
            <> long "config"
            <> metavar "CONFIG"
            <> help "Path to config TOML file"

filePathP :: Parser FilePath
filePathP =
    strOption $
        short 'f'
            <> long "file"
            <> metavar "FILE"
            <> help "Path to CSV file"

outputPathP :: Parser OutputPath
outputPathP =
    OutputPath <$> strOption outputPathField
  where
    outputPathField =
        short 'o'
            <> long "output"
            <> metavar "OUTPUT"
            <> help "Path to output CSV file"

transformCommand :: Mod CommandFields Command
transformCommand =
    command "transform" $
        info transformP $
            progDesc "Transform CSV file"
  where
    transformP =
        TransformCSV <$> configPathP <*> filePathP <*> outputPathP

diffCommand :: Mod CommandFields Command
diffCommand =
    command "diff" $
        info diffP $
            progDesc "Diff CSV files"
  where
    file = argument str (metavar "FILE")
    diffP = DiffCSV <$> file <*> file

input :: Parser Command
input =
    subparser $
        transformCommand <> diffCommand

getCliCommand :: IO Command
getCliCommand =
    execParser $
        info
            (helper <*> versionOption <*> input)
            (header appVersion <> fullDesc)
  where
    versionOption =
        infoOption appVersion $
            long "version" <> short 'v' <> help "Show version"
