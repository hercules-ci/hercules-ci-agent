{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Task where

import qualified Hercules.API.Id as Id
import Hercules.API.Prelude hiding (id)

-- | A task, typically performed by the agent.
--
-- The phantom represents the type of task. See 'AnyTask'.
data Task a
  = Task
      { typ :: Text,
        id :: Id (Task a)
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

-- | @'Task' 'Any' represents tasks whose type has not yet been
-- read.
data Any

-- | Safe type cast.
upcast :: Task a -> Task Any
upcast = uncheckedCast

-- | Safe type cast.
upcastId :: Id (Task a) -> Id (Task Any)
upcastId = Id.uncheckedCast

uncheckedCast :: Task a -> Task b
uncheckedCast a = a {id = Id.uncheckedCast (id a)}
