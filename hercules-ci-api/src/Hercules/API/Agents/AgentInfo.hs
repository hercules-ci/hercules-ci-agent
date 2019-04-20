{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.AgentInfo where

import           Hercules.API.Prelude

data AgentInfo = AgentInfo
  { hostname :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , platforms :: [Text]
  , systemFeatures :: [Text]
  , cachixPushCaches :: [Text]
  , substituters :: [Text]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
