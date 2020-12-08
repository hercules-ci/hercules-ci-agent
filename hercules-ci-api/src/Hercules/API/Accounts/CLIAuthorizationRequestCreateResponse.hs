{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIAuthorizationRequestCreateResponse where

import Hercules.API.Prelude

data CLIAuthorizationRequestCreateResponse = CLIAuthorizationRequestCreateResponse
  { temporaryCLIToken :: Text,
    browserURL :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
