{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.AgentInfo where

import Control.Applicative
import Control.Lens (at, (%~))
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Data.Function
import Hercules.API.Prelude

data AgentInfo = AgentInfo
  { hostname :: Text,
    agentVersion :: Text,
    nixVersion :: Text,
    nixClientProtocolVersion :: Int,
    nixDaemonProtocolVersion :: Int,
    platforms :: [Text],
    systemFeatures :: [Text],
    cachixPushCaches :: [Text],
    pushCaches :: [Text],
    substituters :: [Text],
    concurrentTasks :: Int,
    labels :: Map Text A.Value
  }
  deriving (Generic, Show, Eq, NFData, ToJSON)

instance FromJSON AgentInfo where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup v =
        v
          & _Object . at "concurrentTasks" %~ (<|> Just (A.Number 2))
          & _Object . at "pushCaches" %~ (<|> Just (A.Array mempty))
          & _Object . at "nixClientProtocolVersion" %~ (<|> Just (A.Number 0))
          & _Object . at "nixDaemonProtocolVersion" %~ (<|> Just (A.Number 0))
          & _Object . at "labels" %~ (<|> Just (A.Object mempty))
