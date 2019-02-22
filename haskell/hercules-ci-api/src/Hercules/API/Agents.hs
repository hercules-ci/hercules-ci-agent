{-# LANGUAGE DataKinds #-}
module Hercules.API.Agents where

import           Servant.API
import           Servant.API.Generic
import           Hercules.API.Prelude
import           Hercules.API.Accounts.Account  ( Account )
import           Hercules.API.Agents.Agent      ( Agent )
import           Hercules.API.Agents.AgentToken ( AgentToken )
import           Hercules.API.Agents.CreateAgentToken
                                                ( CreateAgentToken )
import           Hercules.API.Agents.CreateAgent
                                                ( CreateAgent )
import           Hercules.API.Agents.FullAgentToken
                                                ( FullAgentToken )

data AgentsAPI auth f = AgentsAPI
  { agentTokensByAccount :: f :-
      Summary "List all agent tokens owned by an account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "agentTokens" :>
      auth :>
      Get '[JSON] [AgentToken]

  , agentTokenCreate :: f :-
      Summary "Generate a new agent token owned by the account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "agentTokens" :>
      ReqBody '[JSON] CreateAgentToken :>
      auth :>
      Post '[JSON] FullAgentToken

  , agentTokenDelete :: f :-
      Summary "Delete an agent token owned by the account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "agentTokens" :>
      Capture' '[Required, Strict] "agentTokenId" (Id AgentToken) :>
      auth :>
      Delete '[JSON] NoContent

  , agentsByAccount :: f :-
      Summary "Show the agents owned by the account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "agents" :>
      auth :>
      Get '[JSON] [Agent]

  , agentCreate :: f :-
      Summary "Create a new agent." :>
      Description "Authenticated using the agent token acquired through POST /accounts/:accountId/agentTokens" :>
      "agent" :>
      "create" :>
      ReqBody '[JSON] CreateAgent :>
      auth :>
      Post '[JSON] Text

  } deriving Generic
