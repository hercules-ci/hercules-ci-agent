{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.TaskState where

import           Prelude                 hiding ( id )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Swagger                   ( ToSchema )

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
