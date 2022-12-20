{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Hercules.API.Agent.LifeCycle where

import Hercules.API.Agent.LifeCycle.CreateAgentSession_V2 as CreateAgentSession_V2
  ( CreateAgentSession,
  )
import Hercules.API.Agent.LifeCycle.ServiceInfo
import Hercules.API.Agent.LifeCycle.StartInfo
  ( Hello,
    StartInfo,
  )
import Hercules.API.Prelude
import Servant.API

-- | Agent session and "connection" endpoints
data LifeCycleAPI auth f = LifeCycleAPI
  { -- | This is also available in the client API as 'Hercules.API.Agents.agentSessionCreateV2'
    agentSessionCreate ::
      f
        :- Summary "Create a new agent session."
          :> Description "Authenticated using the cluster join token acquired through POST /accounts/:accountId/clusterJoinTokens"
          :> "agent"
          :> "session"
          :> ReqBody '[JSON] CreateAgentSession_V2.CreateAgentSession
          :> auth
          :> Post '[JSON] Text,
    hello ::
      f
        :- Summary "Update an agent session wrt features, versions, capabilities etc."
          :> Description "Authenticated using the agent session token acquired through agentSessionCreate."
          :> "agent"
          :> "hello"
          :> ReqBody '[JSON] Hello
          :> auth
          :> Post '[JSON] NoContent,
    heartbeat ::
      f
        :- Summary "Update an agent session to indicate liveness."
          :> Description "Authenticated using the agent session token acquired through agentSessionCreate."
          :> "agent"
          :> "heartbeat"
          :> ReqBody '[JSON] StartInfo
          :> auth
          :> Post '[JSON] NoContent,
    goodbye ::
      f
        :- Summary "Report that an agent has stopped."
          :> Description "Authenticated using the agent session token acquired through agentSessionCreate."
          :> "agent"
          :> "goodbye"
          :> ReqBody '[JSON] StartInfo
          :> auth
          :> Post '[JSON] NoContent,
    getServiceInfo ::
      f
        :- Summary "Service version and configuration."
          :> "agent"
          :> "service-info"
          :> Get '[JSON] ServiceInfo
  }
  deriving (Generic)
