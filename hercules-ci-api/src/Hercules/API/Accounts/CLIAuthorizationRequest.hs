{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.CLIAuthorizationRequest where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data CLIAuthorizationRequest = CLIAuthorizationRequest
  { description :: Text,
    creationTime :: UTCTime
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
