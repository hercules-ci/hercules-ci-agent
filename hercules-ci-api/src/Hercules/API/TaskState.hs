{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.TaskState where

import Data.Aeson
  ( FromJSON,
    ToJSON
    )
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Prelude hiding (id)

-- | A task, typically performed by the agent.
--
-- The phantom represents the type of task. See 'AnyTask'.
data TaskState
  = Waiting
  | Ready
  | Dispatched
  | Success
  | DependencyFailure
  | Failure
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
