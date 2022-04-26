module Hercules.Agent.Worker.Env where

import Control.Concurrent.STM (TVar)
import Data.IORef (IORef)
import Data.UUID (UUID)
import Hercules.Agent.Worker.HerculesStore (HerculesStore)
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import Hercules.Agent.WorkerProtocol.Event (Event)
import Hercules.CNix (Ref)
import Hercules.CNix.Store (Store, StorePath)
import Protolude

data HerculesState = HerculesState
  { drvsCompleted :: TVar (Map StorePath (UUID, BuildResult.BuildStatus)),
    drvsInProgress :: IORef (Set StorePath),
    herculesStore :: Ptr (Ref HerculesStore),
    wrappedStore :: Store,
    shortcutChannel :: Chan (Maybe Event),
    extraNixOptions :: [(Text, Text)]
  }
