{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Organizations.PaymentLink where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data PaymentLink = PaymentLink
  { url :: Text,
    productId :: Integer
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
