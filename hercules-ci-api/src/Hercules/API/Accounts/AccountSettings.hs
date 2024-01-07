{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.AccountSettings where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data AccountSettings = AccountSettings
  { -- | When a new repo is created/installed, enable building it?
    enableNewRepos :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
