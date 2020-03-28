module Hercules.Agent.Worker.Build where

import CNix
import Conduit
import Hercules.Agent.Worker.Build.Prefetched
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Command.Build
import Hercules.Agent.WorkerProtocol.Event (Event)
import Protolude

runBuild :: Ptr (Ref NixStore) -> Command.Build.Build -> ConduitT i Event (ResourceT IO) ()
runBuild store build = do
  let extraPaths = Command.Build.inputDerivationOutputPaths build
  for_ extraPaths $ \drv ->
    liftIO $ CNix.ensurePath store drv
  buildResult <- liftIO $ buildDerivation store (toS $ Command.Build.drvPath build) extraPaths
  liftIO $ putErrText $ show buildResult
