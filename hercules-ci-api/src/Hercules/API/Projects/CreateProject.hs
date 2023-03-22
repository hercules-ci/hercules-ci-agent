{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.CreateProject where

import Hercules.API.Prelude
import Hercules.API.Repos.Repo (Repo)

data CreateProject = CreateProject
  { primaryRepoId :: Id Repo,
    enabled :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
