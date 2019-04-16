module Hercules.Agent.Options
  ( Options(..)
  , parse
  )
where

import           Protolude               hiding ( option )

import           Hercules.Agent.CabalInfo       ( herculesAgentVersion )
import           Hercules.Agent.Config          ( Config )
import qualified Hercules.Agent.Config         as Config
import           Options.Applicative

data Options = Options
               { configOverrides :: Endo Config
               , configFile :: Maybe FilePath
               }

parseOptions :: Parser Options
parseOptions = Options <$> parseOverrides <*> parseConfigFile


parseOverrides :: Parser (Endo Config)
parseOverrides = mconcat <$> many
  (update (\x c -> c { Config.herculesApiBaseURL = x })
  <$> strOption
        (long "api-base-url" <> metavar "URL" <> help
          "Root of the Hercules CI API"
        )
  <|> update (\x c -> c { Config.clusterJoinTokenPath = x })
  <$> strOption
        (long "cluster-join-token-path" <> metavar "FILE" <> help
          "Token file that authorizes the agent to join a cluster"
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


parseConfigFile :: Parser (Maybe FilePath)
parseConfigFile = optional $ strOption
  (long "config-file" <> metavar "CONFIG_FILE" <> help
    "File path to the configuration file"
  )

parserInfo :: ParserInfo Options
parserInfo = info
  (parseOptions <**> helper)
  (fullDesc <> progDesc "Accepts tasks from Hercules CI and run them" <> header
    ("hercules-ci-agent " <> toSL herculesAgentVersion)
  )

parse :: IO Options
parse = execParser parserInfo
