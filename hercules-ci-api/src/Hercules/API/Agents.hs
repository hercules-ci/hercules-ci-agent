{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Hercules.API.Agents where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Agents.AgentSession
  ( AgentSession,
  )
import Hercules.API.Agents.ClusterJoinToken
  ( ClusterJoinToken,
  )
import Hercules.API.Agents.CreateClusterJoinToken
  ( CreateClusterJoinToken,
  )
import Hercules.API.Agents.FullClusterJoinToken
  ( FullClusterJoinToken,
  )
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic

data AgentsAPI auth f
  = AgentsAPI
      { clusterJoinTokensByAccount ::
          f
            :- Summary "List all cluster join tokens in an account." -- TODO rename
            :> "accounts"
            :> Capture' '[Required, Strict] "accountId" (Id Account)
            :> "clusterJoinTokens"
            :> auth
            :> Get '[JSON] [ClusterJoinToken],
        clusterJoinTokenCreate ::
          f
            :- Summary "Generate a new cluster join token for agents to be added to this account."
            :> "accounts"
            :> Capture' '[Required, Strict] "accountId" (Id Account)
            :> "clusterJoinTokens"
            :> ReqBody '[JSON] CreateClusterJoinToken
            :> auth
            :> Post '[JSON] FullClusterJoinToken,
        clusterJoinTokenDelete ::
          f
            :- Summary "Delete an cluster join token in the account. No new agents will be able to join this account with the specified token."
            :> "accounts"
            :> Capture' '[Required, Strict] "accountId" (Id Account)
            :> "clusterJoinTokens"
            :> Capture' '[Required, Strict] "clusterJoinTokenId" (Id ClusterJoinToken)
            :> auth
            :> Delete '[JSON] NoContent,
        agentSessionsByAccount ::
          f
            :- Summary "Show the agents sessions owned by the account."
            :> "accounts"
            :> Capture' '[Required, Strict] "accountId" (Id Account)
            :> "agentSessions"
            :> auth
            :> Get '[JSON] [AgentSession]
      }
  deriving (Generic)
