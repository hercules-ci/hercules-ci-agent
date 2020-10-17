{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIAuthorizationRequestStatus where

import Hercules.API.Prelude

data CLIAuthorization
  = CLIAuthorization
      { token :: Text,
        userIdentities :: [Text]
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data CLIAuthorizationStatus = Pending () | Granted CLIAuthorization
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data CLIAuthorizationRequestStatus
  = CLIAuthorizationRequestStatus
      { status :: CLIAuthorizationStatus
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
