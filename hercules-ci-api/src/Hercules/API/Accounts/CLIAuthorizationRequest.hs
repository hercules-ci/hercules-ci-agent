{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIAuthorizationRequest where

import Hercules.API.Prelude

data CLIAuthorizationRequest = CLIAuthorizationRequest
  { description :: Text,
    creationTime :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
