{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.BillingStatus
  ( BillingStatus (..),
    fromText
    )
where

import Hercules.API.Prelude

data BillingStatus
  = Community -- Free plan
  | Trial
  | Active
  | Cancelled
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

fromText :: Text -> Maybe BillingStatus
fromText "Community" = Just Community
fromText "Trial" = Just Trial
fromText "Active" = Just Active
fromText "Cancelled" = Just Cancelled
fromText _ = Nothing
