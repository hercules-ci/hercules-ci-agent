{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Repos.SimpleRepo where

import Hercules.API.Accounts.SimpleAccount (SimpleAccount)
import Hercules.API.Prelude
import Hercules.API.Repos.Repo (Repo)

data SimpleRepo = SimpleRepo
  { id :: Id Repo,
    name :: Name Repo,
    owner :: SimpleAccount,
    displayName :: Text,
    imageURL :: Maybe Text,
    isPublic :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
