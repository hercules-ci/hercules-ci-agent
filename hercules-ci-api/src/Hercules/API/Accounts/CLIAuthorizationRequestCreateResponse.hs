{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.CLIAuthorizationRequestCreateResponse where

import Hercules.API.Prelude

data CLIAuthorizationRequestCreateResponse = CLIAuthorizationRequestCreateResponse
  { temporaryCLIToken :: Text,
    browserURL :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
