{-# LANGUAGE DataKinds #-}

module Hercules.API.Agent.Evaluate where

import Data.UUID
import Hercules.API.Agent.Evaluate.DerivationStatus (DerivationStatus)
import Hercules.API.Agent.Evaluate.EvaluateEvent
  ( EvaluateEvent,
  )
import Hercules.API.Agent.Evaluate.EvaluateTask
  ( EvaluateTask,
  )
import Hercules.API.Prelude
import Hercules.API.Task (Task)
import Servant.API

data EvalAPI auth f = EvalAPI
  { tasksGetEvaluation ::
      f
        :- "tasks"
          :> Capture "taskId" (Id (Task EvaluateTask))
          :> "eval"
          :> auth
          :> Get '[JSON] EvaluateTask,
    tasksUpdateEvaluation ::
      f
        :- "tasks"
          :> Capture "taskId" (Id (Task EvaluateTask))
          :> "eval"
          :> ReqBody '[JSON] [EvaluateEvent]
          :> auth
          :> Post '[JSON] NoContent,
    getDerivationStatus ::
      f
        :- "agent"
          :> "build"
          :> Capture "derivationPath" Text
          :> auth
          :> Get '[JSON] (Maybe DerivationStatus),
    getDerivationStatus2 ::
      f
        :- "agent"
          :> "eval-build"
          :> Capture "derivationPath" Text
          :> auth
          :> Get '[JSON] (Maybe (UUID, DerivationStatus))
  }
  deriving (Generic)
