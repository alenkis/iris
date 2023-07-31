module Cli where

import           Meta                (appVersion)
import           Options.Applicative (CommandFields, Mod, Parser, command,
                                      execParser, fullDesc, header, help,
                                      helper, info, infoOption, long, metavar,
                                      progDesc, short, strOption, subparser)

newtype ConfigPath = ConfigPath FilePath
    deriving (Eq, Show)

data Command
    = TransformCSV ConfigPath FilePath
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

transformCommand :: Mod CommandFields Command
transformCommand =
    command "transform" $
        info transformP $
            progDesc "Transform CSV file"
  where
    transformP =
        TransformCSV <$> configPathP <*> filePathP

input :: Parser Command
input =
    subparser transformCommand

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
