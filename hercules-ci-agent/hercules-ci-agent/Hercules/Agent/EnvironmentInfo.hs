module Hercules.Agent.EnvironmentInfo where

import Control.Lens
  ( (^..),
    (^?),
    to,
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
  ( _Array,
    _Number,
    _String,
    key,
  )
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Hercules.API.Agent.LifeCycle.AgentInfo as AgentInfo
import Hercules.Agent.CabalInfo as CabalInfo
import Hercules.Agent.Cachix.Info as Cachix.Info
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env as Env
import Hercules.Agent.Log
import Network.HostName (getHostName)
import Protolude hiding (to)
import qualified System.Process as Process

extractAgentInfo :: App AgentInfo.AgentInfo
extractAgentInfo = do
  hostname <- liftIO getHostName
  nix <- liftIO getNixInfo
  pushCaches <- Cachix.Info.activePushCaches
  concurrentTasks <- asks (Config.concurrentTasks . Env.config)
  let s = AgentInfo.AgentInfo
        { hostname = toS hostname,
          agentVersion = CabalInfo.herculesAgentVersion, -- TODO: Add git revision
          nixVersion = nixExeVersion nix,
          platforms = nixPlatforms nix,
          cachixPushCaches = pushCaches,
          systemFeatures = nixSystemFeatures nix,
          substituters = nixSubstituters nix, -- TODO: Add cachix substituters
          concurrentTasks = fromIntegral concurrentTasks
        }
  logLocM DebugS $ "Determined environment info: " <> show s
  pure s

data NixInfo
  = NixInfo
      { nixExeVersion :: Text,
        nixPlatforms :: [Text],
        nixSystemFeatures :: [Text],
        nixSubstituters :: [Text],
        nixTrustedPublicKeys :: [Text],
        nixNarinfoCacheNegativeTTL :: Maybe Integer
      }

getNixInfo :: IO NixInfo
getNixInfo = do
  let stdinEmpty = ""
  version <- Process.readProcess "nix" ["--version"] stdinEmpty
  rawJson <- Process.readProcess "nix" ["show-config", "--json"] stdinEmpty
  cfg <-
    case Aeson.eitherDecode (toS rawJson) of
      Left e -> panic $ "Could not parse nix show-config --json: " <> show e
      Right r -> pure r
  pure NixInfo
    { nixExeVersion = T.dropAround isSpace (toSL version),
      nixPlatforms =
        ((cfg :: Aeson.Value) ^.. key "system" . key "value" . _String)
          <> ( cfg
                 ^.. key "extra-platforms"
                 . key "value"
                 . _Array
                 . traverse
                 . _String
             ),
      nixSystemFeatures =
        cfg
          ^.. key "system-features"
          . key "value"
          . _Array
          . traverse
          . _String,
      nixSubstituters =
        cfg
          ^.. key "substituters"
          . key "value"
          . _Array
          . traverse
          . _String
          . to cleanUrl,
      nixTrustedPublicKeys =
        cfg
          ^.. key "trusted-public-keys"
          . key "value"
          . _Array
          . traverse
          . _String
          . to cleanUrl,
      nixNarinfoCacheNegativeTTL =
        cfg
          ^? key "narinfo-cache-negative-ttl"
          . key "value"
          . _Number
          . to floor
    }

cleanUrl :: Text -> Text
cleanUrl t | "@" `T.isInfixOf` t = "<URI censored; might contain secret>"
cleanUrl t = t
