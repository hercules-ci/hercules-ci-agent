{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.ClientInfo.ClientInfo where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude

data ClientInfo = ClientInfo
  { apiBaseUrl :: Text,
    frontendBaseUrl :: Text,
    licensedTo :: Text,
    forges :: [Forge],
    -- | @[]@ for unauthenticated users.
    personalAccounts :: [Account]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
