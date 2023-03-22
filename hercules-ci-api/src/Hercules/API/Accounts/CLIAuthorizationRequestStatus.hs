{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.CLIAuthorizationRequestStatus where

import Hercules.API.Prelude

data CLIAuthorization = CLIAuthorization
  { token :: Text,
    userIdentities :: [Text]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)

data CLIAuthorizationStatus = Pending () | Granted CLIAuthorization
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)

data CLIAuthorizationRequestStatus = CLIAuthorizationRequestStatus
  { status :: CLIAuthorizationStatus
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
