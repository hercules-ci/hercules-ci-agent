{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.State.StateLockAcquireRequest where

import Hercules.API.Prelude

data StateLockAcquireRequest = StateLockAcquireRequest
  { description :: Text,
    exclusive :: Bool,
    parent :: Maybe (Id "StateLockLease")
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
