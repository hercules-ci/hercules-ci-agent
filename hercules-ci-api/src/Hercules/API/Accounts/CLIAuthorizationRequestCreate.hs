{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.CLIAuthorizationRequestCreate where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data CLIAuthorizationRequestCreate = CLIAuthorizationRequestCreate
  { description :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
