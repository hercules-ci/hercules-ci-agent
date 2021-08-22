{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.State.StateLockUpdateRequest where

import Hercules.API.Prelude

data StateLockUpdateRequest = StateLockUpdateRequest
  { description :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
