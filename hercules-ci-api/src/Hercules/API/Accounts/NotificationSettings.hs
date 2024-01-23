{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Accounts.NotificationSettings where

import Data.OpenApi qualified as O3
import Hercules.API.Accounts.SimpleAccount (SimpleAccount)
import Hercules.API.Forge.SimpleForge (SimpleForge)
import Hercules.API.Prelude

data NotificationLevel
  = Ignore
  | All
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data NotificationSetting = NotificationSetting
  { notificationLevel :: Maybe NotificationLevel,
    notificationEmail :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data NotificationAccountOverride = NotificationSettingsOverride
  { account :: SimpleAccount,
    setting :: NotificationSetting
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data AuthorizedEmail = AuthorizedEmail
  { address :: Text,
    isPrimary :: Bool,
    source :: Maybe SimpleForge
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data NotificationSettings = NotificationSettings
  { authorizedEmails :: [AuthorizedEmail],
    defaultSetting :: Maybe NotificationSetting,
    accountOverrides :: [NotificationAccountOverride]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
