{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O0 #-}

module Hercules.API
  ( api,
    servantApi,
    servantClientApi,
    swagger,
    useApi,
    enterApiE,
    API,
    ClientAuth,
    HerculesAPI (..),
    ClientAPI (..),
    HerculesServantAPI,
    AddAPIVersion,
    Id,
    Name,
    Result (..),

    -- * Reexports
    NoContent (..),

    -- * Utilities
    noContent,
  )
where

import Control.Lens
import Control.Monad
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Header)
import Hercules.API.Accounts (AccountsAPI)
import Hercules.API.Agents (AgentsAPI)
import Hercules.API.Build as Client
  ( BuildAPI,
  )
import Hercules.API.ClientInfo (ClientInfoAPI)
import Hercules.API.Effects (EffectsAPI)
import Hercules.API.Forge (ForgeAPI)
import Hercules.API.GitLab (GitLabAPI)
import Hercules.API.Health (HealthAPI)
import Hercules.API.Organizations (OrganizationsAPI)
import Hercules.API.Orphans ()
import Hercules.API.Prelude
import Hercules.API.Projects (ProjectsAPI)
import Hercules.API.Repos (ReposAPI)
import Hercules.API.Result (Result (..))
import Hercules.API.Servant (useApi)
import Hercules.API.State (StateAPI)
import Servant.API
import Servant.Auth
import Servant.Auth.Swagger ()
import Servant.Swagger
import Servant.Swagger.UI.Core (SwaggerSchemaUI)

-- TODO remove health so we get clientapi
data HerculesAPI auth f = HerculesAPI
  { accounts :: f :- ToServantApi (AccountsAPI auth),
    clientInfo :: f :- ToServantApi (ClientInfoAPI auth),
    forges :: f :- ToServantApi (ForgeAPI auth),
    repos :: f :- ToServantApi (ReposAPI auth),
    projects :: f :- ToServantApi (ProjectsAPI auth),
    agents :: f :- ToServantApi (AgentsAPI auth),
    build :: f :- ToServantApi (Client.BuildAPI auth),
    effects :: f :- ToServantApi (EffectsAPI auth),
    health :: f :- ToServantApi (HealthAPI auth),
    organizations :: f :- ToServantApi (OrganizationsAPI auth),
    state :: f :- ToServantApi (StateAPI auth),
    gitlab :: f :- ToServantApi (GitLabAPI auth)
  }
  deriving (Generic)

data ClientAPI auth f = ClientAPI
  { clientAccounts :: f :- ToServantApi (AccountsAPI auth),
    clientClientInfo :: f :- ToServantApi (ClientInfoAPI auth),
    clientForges :: f :- ToServantApi (ForgeAPI auth),
    clientRepos :: f :- ToServantApi (ReposAPI auth),
    clientProjects :: f :- ToServantApi (ProjectsAPI auth),
    clientAgents :: f :- ToServantApi (AgentsAPI auth),
    clientBuild :: f :- ToServantApi (Client.BuildAPI auth),
    clientEffects :: f :- ToServantApi (EffectsAPI auth),
    clientOrganizations :: f :- ToServantApi (OrganizationsAPI auth),
    clientState :: f :- ToServantApi (StateAPI auth),
    clientGitLab :: f :- ToServantApi (GitLabAPI auth)
  }
  deriving (Generic)

type ClientAuth = Auth '[JWT, Cookie] ()

type HerculesServantAPI auth = AddAPIVersion (ToServantApi (HerculesAPI auth))

type ClientServantAPI auth = AddAPIVersion (ToServantApi (ClientAPI auth))

type AddAPIVersion api = "api" :> "v1" :> api

servantApi :: Proxy (HerculesServantAPI auth)
servantApi = Proxy

servantClientApi :: Proxy (ClientServantAPI auth)
servantClientApi = Proxy

type API auth =
  HerculesServantAPI auth
    :<|> "api"
    :> SwaggerSchemaUI "v1" "swagger.json"

api :: Proxy (API auth)
api = Proxy

swagger :: Swagger
swagger =
  toSwagger api'
    & info
      . title
    .~ "Hercules CI API"
    & info
      . version
    .~ "v1"
    & info
      . description
    ?~ "You have reached the Hercules Continuous Integration Application Programming Interface. This user interface provides human friendly access to the various endpoints. To get started with Hercules CI, see hercules-ci.com. Happy building! —the Hercules team"
    & withTags clientProjects "project" "Project and job operations"
    & withTags clientBuild "build" "Build related operations"
    & withTags clientEffects "effect" "Effect related operations"
    & withTags clientState "state" "State files and locks, commonly used with effects and the hci command"
    & withTags clientForges "forge" "Forge operations"
    & withTags clientGitLab "gitlab" "GitLab-specific operations"
    & withTags clientRepos "repo" "Repository operations"
    & withTags clientAgents "agent" "Agent admin and monitoring operations"
    & withTags clientAccounts "account" "Account operations"
    & withTags clientOrganizations "organization" "Organizations and billing operations"
    & withTags clientClientInfo "client" "Ad hoc endpoints for the frontend and perhaps some client-side use cases"
  where
    api' = (servantClientApi @(Auth '[JWT] ()))
    withTags f tag desc = applyTagsFor (subOperations (clientApiProxy f) api') [tag & description ?~ desc]

clientApiProxy :: (ClientAPI (Auth '[JWT] ()) AsApi -> a) -> Proxy ("api" :> "v1" :> a)
clientApiProxy _ = Proxy

-- | 'Control.Monad.void' specialised to 'NoContent' to soothe the
-- compiler that rightfully warns about throwing away a do notation
-- result. By specialising, we make sure that we still get warnings
-- if the result type changes in the future. (We'll get an error)
noContent :: Functor m => m Servant.API.NoContent -> m ()
noContent = void
