{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Organizations.BillingInfo where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

data BillingInfo = BillingInfo
  { nextPayment :: Maybe UTCTime,
    activeUsers :: [Account]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
