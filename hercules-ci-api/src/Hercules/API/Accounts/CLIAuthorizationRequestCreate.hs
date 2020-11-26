{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIAuthorizationRequestCreate where

import Hercules.API.Prelude

data CLIAuthorizationRequestCreate = CLIAuthorizationRequestCreate
  { description :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
