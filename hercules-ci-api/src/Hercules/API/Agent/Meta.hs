{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Hercules.API.Agent.Meta where

import           Servant.API
import           Servant.API.Generic
import           Hercules.API.Prelude
import           Hercules.API.Agents.CreateAgentSession_V2 as CreateAgentSession_V2
                                                ( CreateAgentSession )
import           Hercules.API.Agent.Meta.StartInfo  ( StartInfo
                                                    , Hello
                                                    )

-- | Agent session and "connection" endpoints
data MetaAPI auth f = MetaAPI
  { agentSessionCreate :: f :-
      Summary "Create a new agent session." :>
      Description "Authenticated using the cluster join token acquired through POST /accounts/:accountId/clusterJoinTokens" :>
      "agent" :>
      "session" :>
      ReqBody '[JSON] CreateAgentSession_V2.CreateAgentSession :>
      auth :>
      Post '[JSON] Text
      -- ^ This is also available in the client API as 'Hercules.API.Agents.agentSessionCreateV2'

  , agentSessionHello :: f :-
      Summary "Update an agent session wrt features, versions, capabilities etc." :>
      Description "Authenticated using the agent session token acquired through agentSessionCreate." :>
      "agent" :>
      "hello" :>
      ReqBody '[JSON] Hello :>
      auth :>
      Post '[JSON] NoContent

  , agentSessionHeartbeat :: f :-
      Summary "Update an agent session to indicate liveness." :>
      Description "Authenticated using the agent session token acquired through agentSessionCreate." :>
      "agent" :>
      "heartbeat" :>
      ReqBody '[JSON] StartInfo :>
      auth :>
      Post '[JSON] NoContent

  } deriving Generic
