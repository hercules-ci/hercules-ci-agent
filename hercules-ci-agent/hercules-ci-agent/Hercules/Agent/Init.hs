module Hercules.Agent.Init where

import Hercules.Agent.Cachix.Init qualified
import Hercules.Agent.Compat qualified as Compat
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Config.BinaryCaches qualified as BC
import Hercules.Agent.Env (Env (Env))
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.Netrc.Env qualified as Netrc
import Hercules.Agent.Nix.Init qualified
import Hercules.Agent.SecureDirectory qualified as SecureDirectory
import Hercules.Agent.ServiceInfo qualified as ServiceInfo
import Hercules.Agent.Token qualified as Token
import Hercules.CNix qualified
import Hercules.CNix.Util qualified
import Hercules.CNix.Verbosity qualified
import Katip qualified as K
import Network.HTTP.Client.TLS qualified
import Protolude
import Servant.Auth.Client qualified
import Servant.Client qualified
import System.Directory qualified

newEnv :: Config.FinalConfig -> K.LogEnv -> IO Env
newEnv config logEnv = do
  let withLogging :: K.KatipContextT IO a -> IO a
      withLogging = K.runKatipContextT logEnv () "Init"
  withLogging $ K.logLocM K.DebugS $ "Config: " <> K.logStr (show config :: Text)
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
  serviceInfo <- ServiceInfo.newEnv clientEnv
  pure
    Env
      { manager = manager,
        config = config,
        herculesBaseUrl = baseUrl,
        herculesClientEnv = clientEnv,
        serviceInfo = serviceInfo,
        currentToken = Servant.Auth.Client.Token $ encodeUtf8 token,
        binaryCaches = bcs,
        cachixEnv = cachix,
        socket = panic "Socket not defined yet.", -- Hmm, needs different monad?
        kNamespace = emptyNamespace,
        kContext = mempty,
        kLogEnv = logEnv,
        nixEnv = nix,
        netrcEnv = Netrc.Env Nothing
      }

setupLogging :: Config.FinalConfig -> (K.LogEnv -> IO ()) -> IO ()
setupLogging cfg f = do
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stderr (Compat.katipLevel (Config.logLevel cfg)) K.V2
  let mkLogEnv =
        K.registerScribe "stderr" handleScribe K.defaultScribeSettings
          =<< K.initLogEnv (K.Namespace ["Agent"]) ""
  bracket mkLogEnv K.closeScribes f

emptyNamespace :: K.Namespace
emptyNamespace = K.Namespace []

initCNix :: Config.FinalConfig -> IO ()
initCNix cfg = do
  Hercules.CNix.init
  Hercules.CNix.Verbosity.setVerbosity $ Config.nixVerbosity cfg
  Hercules.CNix.Util.installDefaultSigINTHandler
