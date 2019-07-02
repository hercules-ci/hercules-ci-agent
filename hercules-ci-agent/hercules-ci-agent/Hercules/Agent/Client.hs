{-# OPTIONS_GHC -O0 #-} -- TODO https://github.com/haskell-servant/servant/issues/986
module Hercules.Agent.Client
  ( client
  , tasksClient
  , evalClient
  , lifeCycleClient
  , buildClient
  , logsClient
  )
where

import           Protolude
import qualified Servant.Client
import           Servant.Client                 ( ClientM )
import           Servant.Auth.Client            ( )
import           Servant.Client.Generic         ( AsClientT )
import           Hercules.API                   ( HerculesAPI
                                                , ClientAuth
                                                , AddAPIVersion
                                                , servantApi
                                                , eval
                                                , agentBuild
                                                , agentLifeCycle
                                                , tasks
                                                , useApi
                                                )
import           Hercules.API.Agent.Build       ( BuildAPI )
import           Hercules.API.Agent.Evaluate    ( EvalAPI )
import           Hercules.API.Agent.LifeCycle   ( LifeCycleAPI )
import           Hercules.API.Agent.Tasks       ( TasksAPI )
import           Hercules.API.Logs              ( LogsAPI )
import           Servant.API.Generic

client :: HerculesAPI ClientAuth (AsClientT ClientM)
client = fromServant $ Servant.Client.client (servantApi @ClientAuth)

tasksClient :: TasksAPI ClientAuth (AsClientT ClientM)
tasksClient = useApi tasks $ Hercules.Agent.Client.client

evalClient :: EvalAPI ClientAuth (AsClientT ClientM)
evalClient = useApi eval $ Hercules.Agent.Client.client

buildClient :: BuildAPI ClientAuth (AsClientT ClientM)
buildClient = useApi agentBuild $ Hercules.Agent.Client.client

lifeCycleClient :: LifeCycleAPI ClientAuth (AsClientT ClientM)
lifeCycleClient = useApi agentLifeCycle $ Hercules.Agent.Client.client

logsClient :: LogsAPI () (AsClientT ClientM)
logsClient = fromServant $ Servant.Client.client $ 
  (Proxy @(AddAPIVersion (ToServantApi (LogsAPI ()))))
