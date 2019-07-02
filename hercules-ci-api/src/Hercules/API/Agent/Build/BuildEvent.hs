{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Build.BuildEvent where

import           Hercules.API.Prelude
import           Hercules.API.Agent.Build.BuildEvent.Pushed
                                                ( Pushed )
import           Hercules.API.Agent.Build.BuildEvent.OutputInfo
                                                ( OutputInfo )

data BuildEvent
  = OutputInfo OutputInfo
  | Pushed Pushed
  | Done Bool -- ^ True: successful
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
