{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Hercules.API.Agents where

import           Servant.API
import           Servant.API.Generic
import           Hercules.API.Prelude
import           Hercules.API.Accounts.Account  ( Account )
import           Hercules.API.Agents.AgentSession
                                                ( AgentSession )
import           Hercules.API.Agents.ClusterJoinToken
                                                ( ClusterJoinToken )
import           Hercules.API.Agents.CreateClusterJoinToken
                                                ( CreateClusterJoinToken )
import           Hercules.API.Agents.CreateAgentSession as CreateAgentSession
                                                ( CreateAgentSession )
import           Hercules.API.Agents.CreateAgentSession_V2 as CreateAgentSession_V2
                                                ( CreateAgentSession )
import           Hercules.API.Agents.FullClusterJoinToken
                                                ( FullClusterJoinToken )

data AgentsAPI auth f = AgentsAPI
  { clusterJoinTokensByAccount :: f :-  -- TODO rename
      Summary "List all cluster join tokens in an account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "clusterJoinTokens" :>
      auth :>
      Get '[JSON] [ClusterJoinToken]

  , clusterJoinTokenCreate :: f :-
      Summary "Generate a new cluster join token for agents to be added to this account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "clusterJoinTokens" :>
      ReqBody '[JSON] CreateClusterJoinToken :>
      auth :>
      Post '[JSON] FullClusterJoinToken

  , clusterJoinTokenDelete :: f :-
      Summary "Delete an cluster join token in the account. No new agents will be able to join this account with the specified token." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "clusterJoinTokens" :>
      Capture' '[Required, Strict] "clusterJoinTokenId" (Id ClusterJoinToken) :>
      auth :>
      Delete '[JSON] NoContent

  , agentSessionsByAccount :: f :-
      Summary "Show the agents sessions owned by the account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "agentSessions" :>
      auth :>
      Get '[JSON] [AgentSession]

  , agentSessionCreate :: f :-
      Summary "DEPRECATED" :>
      Description "Session creation by client is unsupported at this time. This endpoint may be removed." :>
      "agentSessions" :>
      ReqBody '[JSON] CreateAgentSession.CreateAgentSession :>
      auth :>
      Post '[JSON] Text

  , agentSessionCreateV2 :: f :-
      Summary "DEPRECATED" :>
      Description "Session creation by client is unsupported at this time. This endpoint may be removed." :>
      "agentSessions2" :>
      ReqBody '[JSON] CreateAgentSession_V2.CreateAgentSession :>
      auth :>
      Post '[JSON] Text
      -- ^ This replicates an endpoint from the agent's 'Hercules.API.Agent.LifeCycle.LifeCycleAPI'

  } deriving Generic

{-# DEPRECATED agentSessionCreate "Session creation by client is unsupported at this time. This endpoint may be removed." #-}
{-# DEPRECATED agentSessionCreateV2 "Session creation by client is unsupported at this time. This endpoint may be removed." #-}
