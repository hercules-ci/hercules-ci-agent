{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.Project where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Accounts.SimpleAccount (SimpleAccount)
import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude
import Hercules.API.Repos.Repo (Repo)

data Project = Project
  { id :: Id Project,
    owner :: SimpleAccount,
    ownerId :: Id Account,
    repoId :: Id Repo,
    enabled :: Bool,
    siteSlug :: Name Forge,
    ownerSlug :: Name Account,
    name :: Name Project,
    slug :: Name Project,
    displayName :: Text,
    imageURL :: Maybe Text,
    -- | True if no authorization is required for retrieving basic
    --   information about a project, such as its existence, name,
    --   job statuses etc.
    isPublic :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
