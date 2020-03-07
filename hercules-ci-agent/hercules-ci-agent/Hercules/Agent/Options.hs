module Hercules.Agent.Options
  ( Options (..),
    Mode (..),
    parse,
  )
where

import Hercules.Agent.CabalInfo (herculesAgentVersion)
import Hercules.Agent.Config (ConfigPath (..))
import Options.Applicative
import Protolude hiding (option)

data Options
  = Options
      { configFile :: ConfigPath,
        mode :: Mode
      }

parseOptions :: Parser Options
parseOptions =
  Options <$> parseConfigPath
    <*> parseMode

data Mode = Test | Run

parseMode :: Parser Mode
parseMode =
  flag Run Test $
    long "test-configuration"
      <> help "Don't start the agent but make sure the configuration is valid."

parseConfigPath :: Parser ConfigPath
parseConfigPath =
  TomlPath
    <$> strOption
      ( long "config" <> metavar "FILE"
          <> help
            "File path to the configuration file (TOML)"
      )

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parseOptions <**> helper)
    ( fullDesc <> progDesc "Accepts tasks from Hercules CI and runs them."
        <> header
          ("hercules-ci-agent " <> toSL herculesAgentVersion)
    )

parse :: IO Options
parse = execParser parserInfo
