{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.BillingStatus
  ( BillingStatus (..),
    toText,
    fromText,
  )
where

import Data.Swagger
import Hercules.API.Prelude

data BillingStatus
  = Community -- Free plan
  | Trial
  | Active
  | Cancelled
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

toText :: BillingStatus -> Text
toText Community = "Community"
toText Trial = "Trial"
toText Active = "Active"
toText Cancelled = "Cancelled"

fromText :: Text -> Maybe BillingStatus
fromText "Community" = Just Community
fromText "Cancelled" = Just Cancelled
fromText "Trial" = Just Trial
fromText "Active" = Just Active
fromText _ = Nothing
