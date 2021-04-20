module Hercules.Agent.EnvironmentInfo where

import Control.Lens
  ( to,
    (^..),
    (^?),
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
  ( key,
    _Array,
    _Number,
    _String,
  )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Hercules.API.Agent.LifeCycle.AgentInfo as AgentInfo
import Hercules.Agent.CabalInfo as CabalInfo
import Hercules.Agent.Cachix.Info as Cachix.Info
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env as Env
import Hercules.Agent.Log
import qualified Hercules.CNix.Store as Store
import Network.HostName (getHostName)
import Protolude hiding (to)
import qualified System.Process as Process

extractAgentInfo :: App AgentInfo.AgentInfo
extractAgentInfo = do
  cfg <- asks Env.config
  hostname <- liftIO getHostName
  nix <- liftIO getNixInfo
  cachixPushCaches <- Cachix.Info.activePushCaches
  pushCaches <- Env.activePushCaches
  nixClientProtocolVersion <- liftIO Store.getClientProtocolVersion
  nixStoreProtocolVersion <- liftIO $ Store.withStore Store.getStoreProtocolVersion
  let s =
        AgentInfo.AgentInfo
          { hostname = toS hostname,
            agentVersion = CabalInfo.herculesAgentVersion, -- TODO: Add git revision
            nixVersion = nixExeVersion nix,
            nixClientProtocolVersion = nixClientProtocolVersion,
            nixDaemonProtocolVersion = nixStoreProtocolVersion,
            platforms = nixPlatforms nix,
            cachixPushCaches = cachixPushCaches,
            pushCaches = pushCaches,
            systemFeatures = nixSystemFeatures nix,
            substituters = nixSubstituters nix, -- TODO: Add cachix substituters
            concurrentTasks = fromIntegral $ Config.concurrentTasks cfg,
            labels = Config.labels cfg
          }
  logLocM DebugS $ "Determined environment info: " <> logStr (show s :: Text)
  pure s

data NixInfo = NixInfo
  { nixExeVersion :: Text,
    nixPlatforms :: [Text],
    nixSystemFeatures :: [Text],
    nixSubstituters :: [Text],
    nixTrustedPublicKeys :: [Text],
    nixNarinfoCacheNegativeTTL :: Maybe Integer,
    nixNetrcFile :: Maybe Text
  }

getNixInfo :: IO NixInfo
getNixInfo = do
  let stdinEmpty = ""
  version <- Process.readProcess "nix" ["--version"] stdinEmpty
  rawJson <- Process.readProcess "nix" ["show-config", "--json"] stdinEmpty
  cfg <-
    case Aeson.eitherDecode (LBS.fromStrict $ encodeUtf8 $ toS rawJson) of
      Left e -> panic $ "Could not parse nix show-config --json: " <> show e
      Right r -> pure r
  pure
    NixInfo
      { nixExeVersion = T.dropAround isSpace (toS version),
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
              . to floor,
        nixNetrcFile =
          cfg
            ^? key "netrc-file"
              . key "value"
              . _String
      }

cleanUrl :: Text -> Text
cleanUrl t | "@" `T.isInfixOf` t = "<URI censored; might contain secret>"
cleanUrl t = t
