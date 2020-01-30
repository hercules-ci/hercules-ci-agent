{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Organizations.Organization where

import qualified Hercules.API.BillingStatus as BillingStatus
import Hercules.API.Prelude

data Organization
  = Organization
      { id :: Id Organization,
        displayName :: Text,
        billingStatus :: BillingStatus.BillingStatus,
        trialStartedOn :: Maybe UTCTime,
        subscriptionStartedOn :: Maybe UTCTime,
        subscriptionCancelledOn :: Maybe UTCTime,
        subscriptionCancelUrl :: Maybe Text,
        subscriptionUpdateUrl :: Maybe Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
