{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Build.BuildEvent.Pushed where

import Hercules.API.Prelude

data Pushed
  = Pushed
      { cache :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
