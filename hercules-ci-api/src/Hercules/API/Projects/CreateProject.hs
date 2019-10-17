{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.CreateProject where

import Hercules.API.Prelude
import Hercules.API.Repos.Repo (Repo)

data CreateProject
  = CreateProject
      { primaryRepoId :: Id Repo,
        enabled :: Bool
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
