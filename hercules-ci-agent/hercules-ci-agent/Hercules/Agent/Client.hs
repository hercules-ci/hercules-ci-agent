{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Servant.API
import Servant.Auth.Client ()
import Servant.Client.Generic (AsClientT)
import Servant.Client.Streaming (ClientM)
import Servant.Client.Streaming qualified

client :: AgentAPI ClientAuth (AsClientT ClientM)
client = fromServant $ Servant.Client.Streaming.client (servantApi @ClientAuth)

tasksClient :: TasksAPI ClientAuth (AsClientT ClientM)
tasksClient = useApi tasks Hercules.Agent.Client.client

evalClient :: EvalAPI ClientAuth (AsClientT ClientM)
evalClient = useApi eval Hercules.Agent.Client.client

buildClient :: BuildAPI ClientAuth (AsClientT ClientM)
buildClient = useApi build Hercules.Agent.Client.client

lifeCycleClient :: LifeCycleAPI ClientAuth (AsClientT ClientM)
lifeCycleClient = useApi lifeCycle Hercules.Agent.Client.client

logsClient :: LogsAPI () (AsClientT ClientM)
logsClient =
  fromServant $
    Servant.Client.Streaming.client
      (Proxy @(AddAPIVersion (ToServantApi (LogsAPI ()))))
