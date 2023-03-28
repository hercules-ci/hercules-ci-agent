{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agents.AgentInfo where

import Control.Applicative
import Control.Lens (at, (%~))
import Data.Aeson qualified as A
import Data.Aeson.Lens (_Object)
import Hercules.API.Prelude

data AgentInfo = AgentInfo
  { hostname :: Text,
    agentVersion :: Text,
    nixVersion :: Text,
    platforms :: [Text],
    systemFeatures :: [Text],
    cachixPushCaches :: [Text],
    substituters :: [Text],
    concurrentTasks :: Int
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, ToSchema)

instance FromJSON AgentInfo where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "concurrentTasks" %~ (<|> Just (A.Number 2))
