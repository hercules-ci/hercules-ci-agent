{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.CLIToken where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

data CLIToken = CLIToken
  { id :: Id "CLIToken",
    description :: Text,
    creationTime :: UTCTime,
    userId :: Id Account
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
