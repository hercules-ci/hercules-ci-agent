{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIAuthorizationRequestStatus where

import Hercules.API.Prelude

data CLIAuthorizationStatus = Pending | Expired | Granted
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data CLIAuthorizationRequestStatus
  = CLIAuthorizationRequestStatus
      { status :: CLIAuthorizationStatus,
        token :: Maybe Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
