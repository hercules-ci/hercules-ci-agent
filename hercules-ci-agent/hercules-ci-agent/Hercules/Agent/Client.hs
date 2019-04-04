module Hercules.Agent.Client
  ( client
  , tasksClient
  , evalClient
  , agentsClient
  , buildClient
  )
where

import           Protolude
import qualified Servant.Client
import           Servant.Client                 ( ClientM )
import           Servant.API.Generic            ( fromServant )
import           Servant.Auth.Client            ( )
import           Servant.Client.Generic         ( AsClientT )
import           Hercules.API                   ( HerculesAPI
                                                , ClientAuth
                                                , servantApi
                                                , eval
                                                , agentBuild
                                                , tasks
                                                , agents
                                                , useApi
                                                )
import           Hercules.API.Agent.Build       ( BuildAPI )
import           Hercules.API.Agent.Evaluate    ( EvalAPI )
import           Hercules.API.Agent.Tasks       ( TasksAPI )
import qualified Hercules.API.Agents

client :: HerculesAPI ClientAuth (AsClientT ClientM)
client = fromServant $ Servant.Client.client (servantApi @ClientAuth)

tasksClient :: TasksAPI ClientAuth (AsClientT ClientM)
tasksClient = useApi tasks $ Hercules.Agent.Client.client

evalClient :: EvalAPI ClientAuth (AsClientT ClientM)
evalClient = useApi eval $ Hercules.Agent.Client.client

buildClient :: BuildAPI ClientAuth (AsClientT ClientM)
buildClient = useApi agentBuild $ Hercules.Agent.Client.client

agentsClient :: Hercules.API.Agents.AgentsAPI ClientAuth (AsClientT ClientM)
agentsClient = useApi agents $ Hercules.Agent.Client.client
