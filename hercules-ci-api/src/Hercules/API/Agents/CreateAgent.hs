{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.CreateAgent where

import           Hercules.API.Prelude

data CreateAgent = CreateAgent
  { hostname :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , architectures :: [Text]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
