{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.AgentInfo where

import Control.Applicative
import Control.Lens ((%~), at)
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Data.Function
import Hercules.API.Prelude

data AgentInfo
  = AgentInfo
      { hostname :: Text,
        agentVersion :: Text,
        nixVersion :: Text,
        platforms :: [Text],
        systemFeatures :: [Text],
        cachixPushCaches :: [Text],
        pushCaches :: [Text],
        substituters :: [Text],
        concurrentTasks :: Int
      }
  deriving (Generic, Show, Eq, ToJSON)

instance FromJSON AgentInfo where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup v =
        v
          & _Object . at "concurrentTasks" %~ (<|> Just (A.Number 2))
          & _Object . at "pushCaches" %~ (<|> Just (A.Array mempty))
