{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.AccountSettingsPatch where

import Hercules.API.Prelude

data AccountSettingsPatch
  = AccountSettingsPatch
      { enableNewRepos :: Maybe Bool
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
