{-# LANGUAGE DataKinds #-}

module Hercules.API.Agent where

import Data.Proxy
import Hercules.API.Agent.Build
  ( BuildAPI,
  )
import Hercules.API.Agent.Evaluate
  ( EvalAPI,
  )
import Hercules.API.Agent.LifeCycle
  ( LifeCycleAPI,
  )
import Hercules.API.Agent.Tasks
  ( TasksAPI,
  )
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic
import Servant.Auth

data AgentAPI auth f
  = AgentAPI
      { tasks :: f :- ToServantApi (TasksAPI auth),
        eval :: f :- ToServantApi (EvalAPI auth),
        build :: f :- ToServantApi (BuildAPI auth),
        lifeCycle :: f :- ToServantApi (LifeCycleAPI auth)
      }
  deriving (Generic)

-- TODO check that we don't have overlapping endpoints and remove cookie
type ClientAuth = Auth '[JWT, Cookie] ()

type AgentServantAPI auth = AddAPIVersion (ToServantApi (AgentAPI auth))

type AddAPIVersion api = "api" :> "v1" :> api

servantApi :: Proxy (AgentServantAPI auth)
servantApi = Proxy

type API auth =
  AgentServantAPI auth

api :: Proxy (API auth)
api = Proxy
