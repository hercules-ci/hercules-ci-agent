{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Build.BuildEvent where

import           Hercules.API.Prelude

data BuildEvent
  = Log Text -- may change
  | Done Bool -- True: successful
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
