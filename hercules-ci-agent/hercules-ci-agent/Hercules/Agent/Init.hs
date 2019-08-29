module Hercules.Agent.Init where

import qualified Hercules.Agent.BinaryCaches as BC
import qualified Hercules.Agent.Cachix.Init
import qualified Hercules.Agent.Config as Config
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Env (Env (Env))
import qualified Hercules.Agent.Nix.Init
import qualified Hercules.Agent.SecureDirectory as SecureDirectory
import qualified Hercules.Agent.Token as Token
import qualified Katip as K
import qualified Network.HTTP.Client.TLS
import Protolude
import qualified Servant.Auth.Client
import qualified Servant.Client
import qualified System.Directory

newEnv :: Config.FinalConfig -> K.LogEnv -> IO Env
newEnv config logEnv = do
  let withLogging :: K.KatipContextT IO a -> IO a
      withLogging = K.runKatipContextT logEnv () "Init"
  withLogging $ K.logLocM K.DebugS $ "Config: " <> show config
  System.Directory.createDirectoryIfMissing True (Config.workDirectory config)
  SecureDirectory.init config
  bcs <- withLogging $ BC.parseFile config
  manager <- Network.HTTP.Client.TLS.newTlsManager
  baseUrl <- Servant.Client.parseBaseUrl $ toS $ Config.herculesApiBaseURL config
  let clientEnv :: Servant.Client.ClientEnv
      clientEnv = Servant.Client.mkClientEnv manager baseUrl
  token <- Token.readTokenFile $ toS $ Config.clusterJoinTokenPath config
  cachix <- withLogging $ Hercules.Agent.Cachix.Init.newEnv config (BC.cachixCaches bcs)
  nix <- Hercules.Agent.Nix.Init.newEnv
  pure Env
    { manager = manager,
      config = config,
      herculesBaseUrl = baseUrl,
      herculesClientEnv = clientEnv,
      currentToken = Servant.Auth.Client.Token $ encodeUtf8 token,
      cachixEnv = cachix,
      kNamespace = emptyNamespace,
      kContext = mempty,
      kLogEnv = logEnv,
      nixEnv = nix
      }

setupLogging :: (K.LogEnv -> IO ()) -> IO ()
setupLogging f = do
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stderr K.DebugS K.V2
  let mkLogEnv =
        K.registerScribe "stderr" handleScribe K.defaultScribeSettings
          =<< K.initLogEnv emptyNamespace ""
  bracket mkLogEnv K.closeScribes f

emptyNamespace :: K.Namespace
emptyNamespace = K.Namespace []
