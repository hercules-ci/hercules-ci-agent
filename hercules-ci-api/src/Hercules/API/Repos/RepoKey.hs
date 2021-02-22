{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Repos.RepoKey where

import Hercules.API.Prelude

data RepoKey = RepoKey
  { siteName :: Text,
    ownerName :: Text,
    repoName :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
