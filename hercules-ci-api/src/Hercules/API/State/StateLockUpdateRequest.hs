{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.State.StateLockUpdateRequest where

import Hercules.API.Prelude

data StateLockUpdateRequest = StateLockUpdateRequest
  { description :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
