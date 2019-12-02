{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Repos.Repo where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

-- | Information about a repository on a connected repository site such as github.
data Repo
  = Repo
      { id :: Id Repo,
        ownerId :: Id Account,
        siteSlug :: Text,
        slug :: Text,
        displayName :: Text,
        imageURL :: Maybe Text,
        isPublic :: Bool,
        -- | An installed repo is one that Hercules has permission to.
        --
        -- A non-installed repo is one that is only visible because of the
        -- authenticated user's credentials.
        isInstalled :: Bool,
        -- | Whether the authenticated user can grant permission to this
        --   repository
        isInstallable :: Bool
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
