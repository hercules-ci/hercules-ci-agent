{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Organizations.Organization where

import Data.OpenApi qualified as O3
import Hercules.API.BillingStatus qualified as BillingStatus
import Hercules.API.Prelude

data Organization = Organization
  { id :: Id Organization,
    displayName :: Text,
    billingStatus :: BillingStatus.BillingStatus,
    trialStartedOn :: Maybe UTCTime,
    subscriptionStartedOn :: Maybe UTCTime,
    subscriptionCancelledOn :: Maybe UTCTime,
    subscriptionCancelUrl :: Maybe Text,
    subscriptionUpdateUrl :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
