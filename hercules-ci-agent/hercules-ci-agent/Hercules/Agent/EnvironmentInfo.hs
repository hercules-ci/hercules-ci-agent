module Hercules.Agent.EnvironmentInfo where

import           Protolude               hiding ( to )

import           Control.Lens                   ( (^..)
                                                , to
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Lens                ( key
                                                , _Array
                                                , _String
                                                )
import           Data.Char                      ( isSpace )
import qualified Data.Text                     as T
import qualified Hercules.API.Agents.AgentInfo
                                               as AgentInfo
import           Hercules.Agent.Env            as Env
import           Hercules.Agent.Cachix.Info    as Cachix.Info
import           Hercules.Agent.CabalInfo      as CabalInfo
import           Hercules.Agent.Log
import           Network.HostName               ( getHostName )
import qualified System.Process                as Process

extractAgentInfo :: App AgentInfo.AgentInfo
extractAgentInfo = do

  hostname <- liftIO getHostName

  nix <- liftIO getNixInfo

  pushCaches <- Cachix.Info.activePushCaches

  let s = AgentInfo.AgentInfo
        { hostname = toS hostname
        , agentVersion = CabalInfo.herculesAgentVersion -- TODO: Add git revision
        , nixVersion = nixExeVersion nix
        , platforms = nixPlatforms nix
        , cachixPushCaches = pushCaches
        , systemFeatures = nixSystemFeatures nix
        , substituters = nixSubstituters nix -- TODO: Add cachix substituters
        }
  logLocM DebugS $ "Determined environment info: " <> show s
  pure s

data NixInfo = NixInfo
 { nixExeVersion :: Text
 , nixPlatforms :: [Text]
 , nixSystemFeatures :: [Text]
 , nixSubstituters :: [Text]
 }

getNixInfo :: IO NixInfo
getNixInfo = do
  let stdinEmpty = ""

  version <- Process.readProcess "nix" ["--version"] stdinEmpty

  rawJson <- Process.readProcess "nix" ["show-config", "--json"] stdinEmpty

  cfg <- case Aeson.eitherDecode (toS rawJson) of
    Left e -> panic $ "Could not parse nix show-config --json: " <> show e
    Right r -> pure r

  pure NixInfo
    { nixExeVersion = T.dropAround isSpace (toSL version)
    , nixPlatforms =
      ((cfg :: Aeson.Value) ^.. key "system" . key "value" . _String)
        <> (cfg
           ^.. key "extra-platforms"
           . key "value"
           . _Array
           . traverse
           . _String
           )
    , nixSystemFeatures = cfg
                          ^.. key "system-features"
                          . key "value"
                          . _Array
                          . traverse
                          . _String
    , nixSubstituters = cfg
                        ^.. key "substituters"
                        . key "value"
                        . _Array
                        . traverse
                        . _String
                        . to cleanUrl
    }

cleanUrl :: Text -> Text
cleanUrl t | "@" `T.isInfixOf` t = "<URI censored; might contain secret>"
cleanUrl t = t
