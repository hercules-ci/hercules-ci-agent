{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Logs.LogHello where

import Hercules.API.Prelude

data LogHello = LogHello {storeProtocolVersion :: !Int, clientProtocolVersion :: !Int}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
