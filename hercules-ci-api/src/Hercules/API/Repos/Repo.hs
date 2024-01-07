{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Repos.Repo where

import Data.OpenApi qualified as O3
import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

-- | Information about a repository on a connected repository site such as github.
data Repo = Repo
  { id :: Id Repo,
    ownerId :: Id Account,
    siteSlug :: Text,
    slug :: Text,
    displayName :: Text,
    imageURL :: Maybe Text,
    isPublic :: Bool,
    defaultBranch :: Maybe Text,
    httpURL :: Text,
    sshURL :: Text,
    -- | An installed repo is one that Hercules has permission to.
    --
    -- A non-installed repo is one that is only visible because of the
    -- authenticated user's credentials.
    isInstalled :: Bool,
    -- | Whether the authenticated user can grant permission to this
    --   repository
    isInstallable :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
