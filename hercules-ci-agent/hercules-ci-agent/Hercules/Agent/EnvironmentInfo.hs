module Hercules.Agent.EnvironmentInfo where

import           Protolude               hiding ( to )

import qualified Cachix.Client.Config          as Cachix.Config
import qualified Cachix.Client.Env             as Cachix.Env
import qualified Cachix.Client.OptionsParser   as Cachix.OptionsParser
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
import           Hercules.Agent.CabalInfo      as CabalInfo
import           Hercules.Agent.Log
import           Network.HostName               ( getHostName )
import qualified System.Process                as Process

import qualified System.Environment

extractAgentInfo :: App AgentInfo.AgentInfo
extractAgentInfo = do

  hostname <- liftIO getHostName

  nix <- liftIO getNixInfo

  pushCaches <- liftIO getCachixInfo

  let s = AgentInfo.AgentInfo
        { hostname = toS hostname
        , agentVersion = CabalInfo.herculesAgentVersion -- TODO: Add git revision
        , nixVersion = nixExeVersion nix
        , platforms = nixPlatforms nix
        , cachixPushCaches = pushCaches
        , systemFeatures = nixSystemFeatures nix
        , substituters = nixSubstituters nix
        }
  logLocM DebugS $ "Determined environment info: " <> show s
  pure s

  -- TODO move into Env
initCachix :: IO Cachix.Env.Env
initCachix = do
  let bogusCommand = ["use", "&"]
  (opts, _cmd) <- System.Environment.withArgs bogusCommand
                                              Cachix.OptionsParser.getOpts
  Cachix.Env.mkEnv opts


getCachixInfo :: IO [Text]
getCachixInfo = do
  env <- initCachix
  let
    pushableName Cachix.Config.BinaryCacheConfig { secretKey = _required, name = name }
      = [name]

    pushCaches = do
      config <- toList $ Cachix.Env.config env
      cache <- toList $ Cachix.Config.binaryCaches config
      pushableName cache

  pure pushCaches

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

  config <- case Aeson.eitherDecode (toS rawJson) of
    Left e -> panic $ "Could not parse nix show-config --json: " <> show e
    Right r -> pure r

  pure NixInfo
    { nixExeVersion = T.dropAround isSpace (toSL version)
    , nixPlatforms =
      ((config :: Aeson.Value) ^.. key "system" . key "value" . _String)
        <> (config
           ^.. key "extra-platforms"
           . key "value"
           . _Array
           . traverse
           . _String
           )
    , nixSystemFeatures = config
                          ^.. key "system-features"
                          . key "value"
                          . _Array
                          . traverse
                          . _String
    , nixSubstituters = config
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
