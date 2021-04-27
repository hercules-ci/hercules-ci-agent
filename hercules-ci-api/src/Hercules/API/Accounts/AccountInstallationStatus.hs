{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.AccountInstallationStatus where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude
import Hercules.API.SourceHostingSite.SourceHostingSite (SourceHostingSite)

data AccountInstallationStatus = AccountInstallationStatus
  { site :: SourceHostingSite,
    account :: Maybe Account,
    isProcessingInstallationWebHook :: Bool,
    secondsSinceInstallationWebHookComplete :: Maybe Int
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
