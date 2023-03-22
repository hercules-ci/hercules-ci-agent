{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.AccountSettingsPatch where

import Hercules.API.Prelude

data AccountSettingsPatch = AccountSettingsPatch
  { enableNewRepos :: Maybe Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
