{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Accounts.NotificationSettingsPatch where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Accounts.NotificationSettings (NotificationSetting)
import Hercules.API.Prelude

data NotificationSettingsPatch = NotificationSettingsPatch
  { defaultSetting :: Maybe NotificationSetting,
    accountOverrides :: Map (Id Account) NotificationSetting
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
