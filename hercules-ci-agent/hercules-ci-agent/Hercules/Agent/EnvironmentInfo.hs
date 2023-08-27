{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.EnvironmentInfo where

import Data.ByteString qualified as BS
import Data.Set qualified as S
import Data.Text qualified as T
import Hercules.API.Agent.LifeCycle.AgentInfo qualified as AgentInfo
import Hercules.Agent.CabalInfo as CabalInfo
import Hercules.Agent.Cachix.Info as Cachix.Info
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Env as Env
import Hercules.Agent.Log
import Hercules.CNix qualified as CNix
import Hercules.CNix.Settings qualified as Settings
import Hercules.CNix.Store (withStoreFromURI)
import Hercules.CNix.Store qualified as Store
import Network.HostName (getHostName)
import Protolude hiding (to)

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
            agentVersion = CabalInfo.herculesAgentVersion,
            nixVersion = nixLibVersion nix,
            nixClientProtocolVersion = nixClientProtocolVersion,
            nixDaemonProtocolVersion = nixStoreProtocolVersion,
            platforms = map fromUtf8Lenient $ nixPlatforms nix,
            cachixPushCaches = cachixPushCaches,
            pushCaches = pushCaches,
            systemFeatures = map fromUtf8Lenient $ nixSystemFeatures nix,
            substituters = map fromUtf8Lenient $ nixSubstituters nix,
            concurrentTasks = fromIntegral $ Config.concurrentTasks cfg,
            labels = Config.labels cfg
          }
  logLocM DebugS $ "Determined environment info: " <> logStr (show s :: Text)
  pure s

data NixInfo = NixInfo
  { nixLibVersion :: Text,
    nixPlatforms :: [ByteString],
    nixSystemFeatures :: [ByteString],
    nixSubstituters :: [ByteString],
    nixTrustedPublicKeys :: [ByteString],
    nixNarinfoCacheNegativeTTL :: Word64,
    nixNetrcFile :: Maybe ByteString
  }
  deriving (Show)

fromUtf8Lenient :: ByteString -> Text
fromUtf8Lenient = decodeUtf8With lenientDecode

getNixInfo :: IO NixInfo
getNixInfo = do
  extraPlatforms <- Settings.getExtraPlatforms
  system <- Settings.getSystem
  systemFeatures <- Settings.getSystemFeatures
  substituters <- Settings.getSubstituters
  trustedPublicKeys <- Settings.getTrustedPublicKeys
  narinfoCacheNegativeTTL <- Settings.getNarinfoCacheNegativeTtl
  netrcFile <- Settings.getNetrcFile
  cleanSubstituters <- ordNub <$> traverse cleanUrl substituters
  pure
    NixInfo
      { nixLibVersion = T.dropAround isSpace (fromUtf8Lenient CNix.nixVersion),
        nixPlatforms = toList (S.singleton system <> extraPlatforms),
        nixSystemFeatures = toList systemFeatures,
        nixSubstituters = cleanSubstituters,
        nixTrustedPublicKeys = trustedPublicKeys,
        nixNarinfoCacheNegativeTTL = narinfoCacheNegativeTTL,
        nixNetrcFile = guard (netrcFile /= "") $> netrcFile
      }

cleanUrl :: ByteString -> IO ByteString
cleanUrl t | "@" `BS.isInfixOf` t = pure "<URI censored; might contain secret>"
cleanUrl t = do
  withStoreFromURI (decodeUtf8With lenientDecode t) Store.storeUri
