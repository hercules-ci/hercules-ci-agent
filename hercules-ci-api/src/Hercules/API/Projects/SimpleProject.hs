{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.SimpleProject where

import Hercules.API.Accounts.SimpleAccount (SimpleAccount)
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)

data SimpleProject = SimpleProject
  { id :: Id Project,
    name :: Name Project,
    owner :: SimpleAccount,
    displayName :: Text,
    imageURL :: Maybe Text,
    -- | True if no authorization is required for retrieving basic
    --   information about a project, such as its existence, name,
    --   job statuses etc.
    isPublic :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
