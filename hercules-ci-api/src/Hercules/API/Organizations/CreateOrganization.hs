{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Organizations.CreateOrganization where

import Data.OpenApi qualified as O3
import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

data CreateOrganization = CreateOrganization
  { displayName :: Text,
    primaryAccountId :: Id Account
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
