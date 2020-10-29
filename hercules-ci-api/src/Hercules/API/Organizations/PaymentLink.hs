{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Organizations.PaymentLink where

import Hercules.API.Prelude

data PaymentLink = PaymentLink
  { url :: Text,
    productId :: Integer
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
