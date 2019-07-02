module Hercules.Agent.Options
  ( Options(..)
  , parse
  )
where

import           Protolude               hiding ( option )

import           Hercules.Agent.CabalInfo       ( herculesAgentVersion )
import           Hercules.Agent.Config          ( ConfigPath(..) )
import           Options.Applicative

data Options = Options
               { configFile :: ConfigPath
               }

parseOptions :: Parser Options
parseOptions = Options <$> parseConfigPath

parseConfigPath :: Parser ConfigPath
parseConfigPath = TomlPath <$> strOption
  (long "config" <> metavar "FILE" <> help
    "File path to the configuration file (TOML)"
  )

parserInfo :: ParserInfo Options
parserInfo = info
  (parseOptions <**> helper)
  (fullDesc <> progDesc "Accepts tasks from Hercules CI and run them" <> header
    ("hercules-ci-agent " <> toSL herculesAgentVersion)
  )

parse :: IO Options
parse = execParser parserInfo
