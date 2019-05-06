{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Build.BuildEvent where

import           Hercules.API.Prelude
import           Hercules.API.Agent.Build.BuildEvent.Pushing
                                                ( Pushing )
import           Hercules.API.Agent.Build.BuildEvent.OutputInfo
                                                ( OutputInfo )

data BuildEvent
  = OutputInfo OutputInfo
  | Pushing Pushing
  | Done Bool -- ^ True: successful
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
