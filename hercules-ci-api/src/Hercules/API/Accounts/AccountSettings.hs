{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.AccountSettings where

import Hercules.API.Prelude

data AccountSettings
  = AccountSettings
      { -- | When a new repo is created/installed, enable building it?
        enableNewRepos :: Bool
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
