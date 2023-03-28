{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Organizations.PaymentLink where

import Hercules.API.Prelude

data PaymentLink = PaymentLink
  { url :: Text,
    productId :: Integer
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
