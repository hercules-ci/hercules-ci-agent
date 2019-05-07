{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Hercules.API
  ( api
  , servantApi
  , swagger
  , useApi
  , API
  , ClientAuth
  , HerculesAPI(..)
  , HerculesServantAPI
  , AddAPIVersion
  , Id
  , Name
  , Result(..)

    -- * Reexports
  , NoContent(..)

    -- * Utilities
  , noContent
  )
where

import           Prelude

import           Control.Lens
import           Control.Monad
import           Data.Proxy                     ( Proxy(..) )
import           Data.Swagger            hiding ( Header )
import           GHC.Generics                   ( Generic )
import           Hercules.API.Accounts          ( AccountsAPI )
import           Hercules.API.Agents            ( AgentsAPI )
import           Hercules.API.Id                ( Id )
import           Hercules.API.Name              ( Name )
import           Hercules.API.Projects          ( ProjectsAPI )
import           Hercules.API.Repos             ( ReposAPI )
import           Servant.API
import           Servant.API.Generic
import           Servant.Auth
import           Servant.Auth.Swagger           ( )
import           Servant.Swagger
import           Servant.Swagger.UI.Core        ( SwaggerSchemaUI )
import           Hercules.API.Agent.Build      as Agent
                                                ( BuildAPI )
import           Hercules.API.Agent.Evaluate   as Agent
                                                ( EvalAPI )
import           Hercules.API.Agent.Tasks      as Agent
                                                ( TasksAPI )
import           Hercules.API.Build            as Client
                                                ( BuildAPI )
import           Hercules.API.Result            ( Result(..) )
import           Hercules.API.Health            ( HealthAPI )

data HerculesAPI auth f = HerculesAPI
   { accounts :: f :- ToServantApi (AccountsAPI auth)
   , repos :: f :- ToServantApi (ReposAPI auth)
   , projects :: f :- ToServantApi (ProjectsAPI auth)
   , agents :: f :- ToServantApi (AgentsAPI auth)
   , tasks :: f :- ToServantApi (TasksAPI auth)
   , eval :: f :- ToServantApi (EvalAPI auth)
   , agentBuild :: f :- ToServantApi (Agent.BuildAPI auth)
   , build :: f :- ToServantApi (Client.BuildAPI auth)
   , health :: f :- ToServantApi (HealthAPI auth)
   } deriving Generic

data ClientAPI auth f = ClientAPI
   { clientAccounts :: f :- ToServantApi (AccountsAPI auth)
   , clientRepos :: f :- ToServantApi (ReposAPI auth)
   , clientProjects :: f :- ToServantApi (ProjectsAPI auth)
   , clientAgents :: f :- ToServantApi (AgentsAPI auth)
   , clientBuild :: f :- ToServantApi (Client.BuildAPI auth)
   } deriving Generic

type ClientAuth = Auth '[JWT, Cookie] ()

type HerculesServantAPI auth = AddAPIVersion (ToServantApi (HerculesAPI auth))
type ClientServantAPI auth = AddAPIVersion (ToServantApi (ClientAPI auth))
type AddAPIVersion api = "api" :> "v1" :> api

servantApi :: Proxy (HerculesServantAPI auth)
servantApi = Proxy

servantClientApi :: Proxy (ClientServantAPI auth)
servantClientApi = Proxy

type API auth = (HerculesServantAPI auth)
   :<|> "api" :> SwaggerSchemaUI "v1" "swagger.json"

api :: Proxy (API auth)
api = Proxy

swagger :: Swagger
swagger =
  toSwagger (servantClientApi @(Auth '[JWT] ()))
    & info
    . title
    .~ "Hercules CI API"
    & info
    . version
    .~ "v1"
    & info
    . description
    ?~ "You have reached the Hercules Continuous Integration Application Programming Interface. This user interface provides human friendly access to the various endpoints. To get started with Hercules CI, see hercules-ci.com. Happy building! —the Hercules team"

-- | Postcomposes 'Servant.API.Generic.fromServant' to an accessor,
-- preserving the mode parameter, because otherwise the mode parameter
-- can not be inferred.
--
-- Ideally, this functionality would be built into a new combinator.
useApi :: (GenericServant f mode, GenericServant g mode)
       => (f mode -> ToServant g mode)
       -> f mode
       -> g mode
useApi = (Servant.API.Generic.fromServant .)

-- | 'Control.Monad.void' specialised to 'NoContent' to soothe the
-- compiler that rightfully warns about throwing away a do notation
-- result. By specialising, we make sure that we still get warnings
-- if the result type changes in the future. (We'll get an error)
noContent :: Functor m => m Servant.API.NoContent -> m ()
noContent = void
