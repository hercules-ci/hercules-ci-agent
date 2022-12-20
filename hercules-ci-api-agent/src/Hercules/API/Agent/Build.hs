{-# LANGUAGE DataKinds #-}

module Hercules.API.Agent.Build where

import Data.ByteString (ByteString)
import qualified Hercules.API.Agent.Build.BuildEvent as BuildEvent
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import Hercules.API.Prelude
import qualified Hercules.API.Task as Task
import Servant.API

data BuildAPI auth f = BuildAPI
  { getBuild ::
      f
        :- "tasks"
          :> Capture "taskId" (Id (Task.Task BuildTask.BuildTask))
          :> "build"
          :> auth
          :> Get '[JSON] BuildTask.BuildTask,
    updateBuild ::
      f
        :- "tasks"
          :> Capture "taskId" (Id (Task.Task BuildTask.BuildTask))
          :> "build"
          :> ReqBody '[JSON] [BuildEvent.BuildEvent]
          :> auth
          :> Post '[JSON] NoContent,
    writeBuildLog ::
      f
        :- Summary "DEPRECATED"
          :> "tasks"
          :> Capture "taskId" (Id (Task.Task BuildTask.BuildTask))
          :> "build"
          :> "_log"
          :> ReqBody '[OctetStream] ByteString
          :> auth
          :> Post '[JSON] NoContent
  }
  deriving (Generic)

{-# DEPRECATED writeBuildLog "Use 'Hercules.API.Logs.writeLog' instead" #-}
