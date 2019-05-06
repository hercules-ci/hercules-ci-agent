{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Build.BuildEvent.Pushing where

import Hercules.API.Prelude

-- FIXME: rename to pushed
data Pushing = Pushing
 { cache :: Text
 } deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
