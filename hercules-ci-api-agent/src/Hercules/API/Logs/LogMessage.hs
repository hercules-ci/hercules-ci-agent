{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Logs.LogMessage where

import Data.Vector
import Data.Word (Word64)
import Hercules.API.Logs.LogEntry
import Hercules.API.Prelude

data LogMessage
  = LogEntries (Vector LogEntry)
  | Start {storeProtocolVersion :: !Int, clientProtocolVersion :: !Int}
  | End {i :: !Word64, ms :: !Word64}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
