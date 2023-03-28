{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.CLITokensResponse where

import Hercules.API.Accounts.CLIToken (CLIToken)
import Hercules.API.Prelude

data CLITokensResponse = CLITokensResponse
  { cliTokens :: [CLIToken]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
