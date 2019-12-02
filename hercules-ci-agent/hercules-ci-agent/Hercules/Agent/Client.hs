{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}

-- TODO https://github.com/haskell-servant/servant/issues/986
module Hercules.Agent.Client
  ( client,
    tasksClient,
    evalClient,
    lifeCycleClient,
    buildClient,
    logsClient,
  )
where

import Hercules.API.Agent (AddAPIVersion, AgentAPI, ClientAuth, build, eval, lifeCycle, servantApi, tasks)
import Hercules.API.Agent.Build (BuildAPI)
import Hercules.API.Agent.Evaluate (EvalAPI)
import Hercules.API.Agent.LifeCycle (LifeCycleAPI)
import Hercules.API.Agent.Tasks (TasksAPI)
import Hercules.API.Logs (LogsAPI)
import Hercules.API.Servant (useApi)
import Protolude
import Servant.API.Generic
import Servant.Auth.Client ()
import qualified Servant.Client
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT)

client :: AgentAPI ClientAuth (AsClientT ClientM)
client = fromServant $ Servant.Client.client (servantApi @ClientAuth)

tasksClient :: TasksAPI ClientAuth (AsClientT ClientM)
tasksClient = useApi tasks $ Hercules.Agent.Client.client

evalClient :: EvalAPI ClientAuth (AsClientT ClientM)
evalClient = useApi eval $ Hercules.Agent.Client.client

buildClient :: BuildAPI ClientAuth (AsClientT ClientM)
buildClient = useApi build $ Hercules.Agent.Client.client

lifeCycleClient :: LifeCycleAPI ClientAuth (AsClientT ClientM)
lifeCycleClient = useApi lifeCycle $ Hercules.Agent.Client.client

logsClient :: LogsAPI () (AsClientT ClientM)
logsClient =
  fromServant $ Servant.Client.client $
    (Proxy @(AddAPIVersion (ToServantApi (LogsAPI ()))))
