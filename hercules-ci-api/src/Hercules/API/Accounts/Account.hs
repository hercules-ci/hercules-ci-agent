{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.Account where

import Hercules.API.Prelude
import Prelude

data AccountType = User | Organization
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data Account
  = Account
      { id :: Id Account,
        sourceHostingSiteSlug :: Text,
        slug :: Text,
        typ :: AccountType,
        displayName :: Text,
        imageURL :: Text,
        isInstalled :: Bool,
        -- ^ Whether Hercules CI is installed on this account as an App.
        --
        -- An account that does not have an installation can not be
        -- properly accessed by Hercules, but may be visible nonetheless
        -- at times because of OAuth scopes.
        --
        -- As an example, non-installed accounts show up when a GitHub
        -- user signs in for the first time via OAuth, until they decide
        -- to install it on their GitHub user. Another example is GitHub
        -- organizations that don't have an installation yet.
        --
        isInstallable :: Bool
        -- ^ Whether the current user has permission in the to installing
        -- Hercules CI on this account.
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
