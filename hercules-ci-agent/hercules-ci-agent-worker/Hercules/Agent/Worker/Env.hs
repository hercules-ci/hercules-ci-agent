module Hercules.Agent.Worker.Env where

import Control.Concurrent.STM (TVar)
import Data.IORef (IORef)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hercules.Agent.Worker.HerculesStore (HerculesStore)
import Hercules.Agent.WorkerProtocol.Command.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.Event (Event)
import Hercules.CNix (Ref)
import Hercules.CNix.Store (Store, StorePath)
import Hercules.CNix.Verbosity qualified as CNix
import Protolude

data HerculesState = HerculesState
  { drvsCompleted :: TVar (Map StorePath (UUID, BuildResult.BuildStatus)),
    drvBuildAsyncs :: TVar (Map StorePath (Async (UUID, BuildResult.BuildStatus))),
    drvRebuildAsyncs :: TVar (Map StorePath (Async (UUID, BuildResult.BuildStatus))),
    drvOutputSubstituteAsyncs :: TVar (Map (StorePath, ByteString) (Async StorePath)),
    drvsInProgress :: IORef (Set StorePath),
    herculesStore :: Ptr (Ref HerculesStore),
    wrappedStore :: Store,
    sendEvents :: Vector Event -> IO (),
    extraNixOptions :: [(Text, Text)],
    nixVerbosity :: CNix.Verbosity
  }
