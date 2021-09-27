{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.SimpleAccount where

import Hercules.API.Accounts.Account (Account, AccountType)
import Hercules.API.Prelude
import Hercules.API.SourceHostingSite.SimpleSite (SimpleSite)

data SimpleAccount = SimpleAccount
  { id :: Id Account,
    name :: Name Account,
    displayName :: Text,
    typ :: AccountType,
    imageURL :: Text,
    site :: SimpleSite
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
