{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.CreateAgentToken where

import           Hercules.API.Prelude

-- owner is in the path
data CreateAgentToken = CreateAgentToken
  { description :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
