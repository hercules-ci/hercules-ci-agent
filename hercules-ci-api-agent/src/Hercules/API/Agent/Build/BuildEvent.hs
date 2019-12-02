{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Build.BuildEvent where

import Hercules.API.Agent.Build.BuildEvent.OutputInfo
  ( OutputInfo,
  )
import Hercules.API.Agent.Build.BuildEvent.Pushed
  ( Pushed,
  )
import Hercules.API.Prelude

data BuildEvent
  = OutputInfo OutputInfo
  | Pushed Pushed
  | -- | True: successful
    Done Bool
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
