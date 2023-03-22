{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.CLIAuthorizationRequest where

import Hercules.API.Prelude

data CLIAuthorizationRequest = CLIAuthorizationRequest
  { description :: Text,
    creationTime :: UTCTime
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
