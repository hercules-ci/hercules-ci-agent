{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.SimpleAccount where

import Hercules.API.Accounts.Account (Account, AccountType)
import Hercules.API.Forge.SimpleForge (SimpleForge)
import Hercules.API.Prelude

data SimpleAccount = SimpleAccount
  { id :: Id Account,
    name :: Name Account,
    displayName :: Text,
    typ :: AccountType,
    imageURL :: Text,
    site :: SimpleForge
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
