{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.StartInfo where

import Control.Applicative
import Control.Lens (at, (%~))
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Hercules.API.Agent.LifeCycle.AgentInfo (AgentInfo)
import Hercules.API.Prelude
import Hercules.API.Task

data Hello = Hello
  { agentInfo :: AgentInfo,
    startInfo :: StartInfo,
    tasksInProgress :: [Id (Task Any)]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON)

instance FromJSON Hello where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "tasksInProgress" %~ (<|> Just (A.Array mempty))

data StartInfo = StartInfo
  { id :: Id StartInfo,
    startTime :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
