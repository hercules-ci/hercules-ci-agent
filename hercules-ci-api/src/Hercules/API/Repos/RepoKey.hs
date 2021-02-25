{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Repos.RepoKey where

import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)

data RepoKey = RepoKey
  { siteName :: Text,
    ownerName :: Text,
    repoName :: Text,
    projectId :: Maybe (Id Project)
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
