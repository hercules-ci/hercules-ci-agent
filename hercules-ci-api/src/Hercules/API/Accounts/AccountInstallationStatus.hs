{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.AccountInstallationStatus where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude

data AccountInstallationStatus = AccountInstallationStatus
  { site :: Forge,
    account :: Maybe Account,
    isProcessingInstallationWebHook :: Bool,
    secondsSinceInstallationWebHookComplete :: Maybe Int
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
