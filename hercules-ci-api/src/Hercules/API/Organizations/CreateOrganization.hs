{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Organizations.CreateOrganization where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

data CreateOrganization
  = CreateOrganization
      { displayName :: Text,
        primaryAccountId :: Id Account
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
