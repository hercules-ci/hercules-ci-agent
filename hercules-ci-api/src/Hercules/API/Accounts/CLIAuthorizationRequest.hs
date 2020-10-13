{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIAuthorizationRequest where

import Hercules.API.Prelude

data CLIAuthorizationRequest
  = CLIAuthorizationRequest
      { hostname :: Text,
        temporaryToken :: Text,
        publicKey :: Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
