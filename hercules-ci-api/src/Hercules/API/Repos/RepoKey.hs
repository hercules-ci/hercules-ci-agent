{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Repos.RepoKey where

import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)

data RepoKey = RepoKey
  { siteName :: Text,
    ownerName :: Text,
    repoName :: Text,
    projectId :: Maybe (Id Project)
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
