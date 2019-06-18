module Hercules.Agent.Options
  ( Options(..)
  , parse
  )
where

import           Protolude               hiding ( option )

import           Hercules.Agent.CabalInfo       ( herculesAgentVersion )
import           Hercules.Agent.Config          ( Config
                                                , ConfigPath(..)
                                                )
import qualified Hercules.Agent.Config         as Config
import           Options.Applicative

data Options = Options
               { configOverrides :: Endo Config
               , configFile :: Maybe ConfigPath
               }

parseOptions :: Parser Options
parseOptions = Options <$> parseOverrides <*> parseConfigFile


parseOverrides :: Parser (Endo Config)
parseOverrides = mconcat <$> many
  (update (\x c -> c { Config.herculesApiBaseURL = Just x })
  <$> strOption
        (long "api-base-url" <> metavar "URL" <> help
          "Root of the Hercules CI API"
        )
  <|> update (\x c -> c { Config.clusterJoinTokenPath = x })
  <$> strOption
        (long "cluster-join-token-path" <> metavar "FILE" <> help
          "Token file that authorizes the agent to join a cluster"
        )
  <|> update (\x c -> c { Config.cacheKeysPath = Just x })
  <$> strOption
        (long "cache-keys-path"
        <> metavar "FILE"
        <> help
             "JSON file with secrets called CacheKeys to access this agent's binary caches"
          -- TODO (doc) CacheKeys JSON reference link
        )
  <|> update (\x c -> c { Config.concurrentTasks = x })
  <$> option
        auto
        (long "concurrent-tasks"
        <> metavar "N"
        <> help
             "Number of tasks to perform simultaneously, such as evaluations, derivations"
        )
  )
 where
  update :: (a -> b -> b) -> a -> Endo b
  update f a = Endo (f a)


parseConfigFile :: Parser (Maybe ConfigPath)
parseConfigFile = optional
  (TomlPath
  <$> strOption
        (long "config-toml" <> metavar "FILE" <> help
          "File path to the configuration file (TOML)"
        )
  <|> JsonPath
  <$> strOption
        (long "config-json" <> metavar "FILE" <> help
          "File path to the configuration file (JSON)"
        )
  )

parserInfo :: ParserInfo Options
parserInfo = info
  (parseOptions <**> helper)
  (fullDesc <> progDesc "Accepts tasks from Hercules CI and run them" <> header
    ("hercules-ci-agent " <> toSL herculesAgentVersion)
  )

parse :: IO Options
parse = execParser parserInfo
