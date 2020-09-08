{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.ServiceInfo
  ( ServiceInfo (..),
  )
where

import Control.DeepSeq (NFData)
import Hercules.API.Prelude

data ServiceInfo
  = ServiceInfo
      { version :: (Int, Int),
        agentSocketBaseURL :: Text,
        bulkSocketBaseURL :: Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
