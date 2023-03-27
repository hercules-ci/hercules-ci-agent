module Hercules.Agent.ServiceInfo where

import Hercules.API.Agent.LifeCycle qualified as API.LifeCycle
import Hercules.API.Agent.LifeCycle.ServiceInfo qualified as ServiceInfo
import Hercules.Agent.Client qualified as Client
import Hercules.Error (escalate)
import Network.URI
import Protolude
import Servant.Client.Streaming qualified

data Env = Env
  { agentSocketBaseURL :: URI,
    bulkSocketBaseURL :: URI
  }

newEnv :: Servant.Client.Streaming.ClientEnv -> IO Env
newEnv clientEnv = do
  serviceInfo <- escalate =<< Servant.Client.Streaming.runClientM (API.LifeCycle.getServiceInfo Client.lifeCycleClient) clientEnv
  let parse varName text = case parseURI (toS text) of
        Nothing -> panic $ varName <> " invalid: " <> ServiceInfo.agentSocketBaseURL serviceInfo
        Just uri ->
          uri <$ do
            case uriScheme uri of
              "http:" -> pass
              "https:" -> pass
              x -> panic $ varName <> " has invalid uri scheme" <> toS x
  agentSocketBaseURL_ <- parse "agentSocketBaseURL" $ ServiceInfo.agentSocketBaseURL serviceInfo
  bulkSocketBaseURL_ <- parse "bulkSocketBaseURL" $ ServiceInfo.bulkSocketBaseURL serviceInfo
  pure
    Env
      { agentSocketBaseURL = agentSocketBaseURL_,
        bulkSocketBaseURL = bulkSocketBaseURL_
      }
