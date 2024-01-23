{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.BillingStatus
  ( BillingStatus (..),
    toText,
    fromText,
  )
where

import Data.OpenApi qualified as O3
import Data.Swagger
import Hercules.API.Prelude

data BillingStatus
  = Community -- Free plan
  | Trial
  | Active
  | Cancelled
  | External
  | Enterprise
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

toText :: BillingStatus -> Text
toText Community = "Community"
toText Trial = "Trial"
toText Active = "Active"
toText Cancelled = "Cancelled"
toText External = "External"
toText Enterprise = "Enterprise"

fromText :: Text -> Maybe BillingStatus
fromText "Community" = Just Community
fromText "Cancelled" = Just Cancelled
fromText "Trial" = Just Trial
fromText "Active" = Just Active
fromText "External" = Just External
fromText "Enterprise" = Just Enterprise
fromText _ = Nothing
