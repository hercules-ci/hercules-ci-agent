{-# LANGUAGE DataKinds #-}

module Hercules.API.Agent.Tasks where

import Data.Aeson (Object)
import Hercules.API.Prelude
import qualified Hercules.API.Task as Task
import Hercules.API.TaskStatus
import Servant.API
import Servant.API.Generic

data TasksAPI auth f
  = TasksAPI
      { tasksReady
          :: f
               :- "tasks"
               :> auth
               :> Post '[JSON] (Maybe (Task.Task Task.Any)),
        tasksSetStatus
          :: f
               :- "tasks"
               :> Capture "taskId" (Id (Task.Task Task.Any))
               :> ReqBody '[JSON] TaskStatus
               :> auth
               :> Post '[JSON] NoContent,
        postLog
          :: f
               :- "tasks"
               :> "log"
               :> ReqBody '[JSON] [Object]
               :> auth
               :> Post '[JSON] NoContent
        }
  deriving (Generic)
