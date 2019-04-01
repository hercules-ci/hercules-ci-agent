module Hercules.Agent.Init where

import           Protolude

import qualified Hercules.Agent.Config         as Config
import qualified Hercules.Agent.Env            as Env
import           Hercules.Agent.Env             ( Env(Env) )
import qualified Network.HTTP.Client.TLS
import qualified Servant.Client
import qualified Servant.Auth.Client
import qualified Katip                         as K
import qualified Hercules.Agent.Token          as Token

newEnv :: Config.Config -> K.LogEnv -> IO Env
newEnv config logEnv = do
  let endpoint = Config.herculesApiBaseURL config
  manager <- Network.HTTP.Client.TLS.newTlsManager
  baseUrl <- Servant.Client.parseBaseUrl (toS endpoint)
  let clientEnv :: Servant.Client.ClientEnv
      clientEnv = Servant.Client.mkClientEnv manager baseUrl
  token <- Token.readTokenFile $ toS $ Config.clusterJoinTokenPath config
  pure Env
    { manager = manager
    , herculesBaseUrl = baseUrl
    , herculesClientEnv = clientEnv
    , currentToken = Servant.Auth.Client.Token $ encodeUtf8 token
    , kNamespace = emptyNamespace
    , kContext = mempty
    , kLogEnv = logEnv
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
