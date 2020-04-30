module Hercules.Agent.AgentSocket (withAgentSocket) where

import qualified Data.Map as M
import Hercules.API.Agent.LifeCycle.StartInfo (Hello, tasksInProgress)
import qualified Hercules.API.Agent.Socket.AgentPayload as AgentPayload
import Hercules.API.Agent.Socket.AgentPayload (AgentPayload)
import qualified Hercules.API.Agent.Socket.ServicePayload as ServicePayload
import Hercules.API.Agent.Socket.ServicePayload (ServicePayload)
import Hercules.API.Id (Id)
import Hercules.API.Task (Task)
import qualified Hercules.API.Task as Task
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env
  ( App,
  )
import qualified Hercules.Agent.Env as Agent.Env
import Hercules.Agent.STM (TVar, readTVarIO)
import Hercules.Agent.Socket as Socket
import Protolude hiding
  ( bracket,
    forkFinally,
    handle,
    killThread,
    race,
    retry,
    withMVar,
  )
import qualified Servant.Auth.Client

withAgentSocket :: Hello -> TVar (Map (Id (Task Task.Any)) b) -> (Socket ServicePayload AgentPayload -> App a) -> App a
withAgentSocket hello tasks f = do
  let setSocket socket env = env {Agent.Env.socket = socket}
      appThread socket = local (setSocket socket) $ f socket
      checkAgentVersion (ServicePayload.ServiceInfo si) = checkVersion' si
      checkAgentVersion _ = throwIO $ FatalError "Unexpected message. This is either a bug or you might need to update your agent."
  base <- asks (Config.agentSocketBase . Agent.Env.config)
  agentToken <- asks (Servant.Auth.Client.getToken . Agent.Env.currentToken)
  withReliableSocket
    ( Socket.SocketConfig
        { makeHello = do
            currentTasks <- readTVarIO tasks
            pure $ AgentPayload.Hello hello {tasksInProgress = M.keys currentTasks},
          checkVersion = checkAgentVersion,
          host = base,
          path = "/api/v1/agent-socket",
          token = agentToken
        }
    )
    appThread
