{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.TaskEvent where

import           Prelude                 hiding ( id )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Hercules.API.Id                ( Id )
import           Hercules.API.Task              ( Task )
import           Hercules.API.TaskState         ( TaskState )
import           Data.Swagger                   ( ToSchema )

-- | A task, typically performed by the agent.
--
-- The phantom represents the type of task. See 'AnyTask'.
data TaskEvent a = TaskEvent
  { typ :: Text
  , id :: Id (Task a)
  , newState :: TaskState
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
