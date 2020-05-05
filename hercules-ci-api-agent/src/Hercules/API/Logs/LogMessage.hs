{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Logs.LogMessage where

import Data.Vector
import Data.Word (Word64)
import Hercules.API.Logs.LogEntry
import Hercules.API.Prelude

-- | Used as a payload on the Hercules CI "reliable socket" protocol; it goes in 'Hercules.API.Agent.Socket.Frame.p'.
data LogMessage
  = LogEntries (Vector LogEntry)
  | -- | End of log stream. For @i@ and @ms@ are like 'LogEntry'
    End {i :: !Word64, ms :: !Word64}
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
