{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Build where

import CNix
import CNix.Internal.Context (Derivation)
import Cachix.Client.Store (Store, queryPathInfo, validPathInfoNarHash, validPathInfoNarSize)
import Conduit
import Data.Conduit.Katip.Orphans ()
import Foreign (ForeignPtr)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Command.Build
import Hercules.Agent.WorkerProtocol.Event (Event)
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.BuildResult as Event.BuildResult
import Katip
import Protolude hiding (yield)
import Unsafe.Coerce

runBuild :: (MonadIO m, KatipContext m) => Ptr (Ref NixStore) -> Command.Build.Build -> ConduitT i Event m ()
runBuild store build = do
  let extraPaths = Command.Build.inputDerivationOutputPaths build
      drvPath = encodeUtf8 $ Command.Build.drvPath build
  x <- for extraPaths $ \input -> do
    liftIO $ try $ CNix.ensurePath store input
  materialize <- case sequenceA x of
    Right _ ->
      -- no error, proceed with requested materialization setting
      pure $ Command.Build.materializeDerivation build
    Left (e :: SomeException) -> liftIO do
      CNix.logInfo $ "while retrieving dependencies: " <> toS (displayException e)
      CNix.logInfo "unable to retrieve dependency; will build locally"
      pure True
  derivationMaybe <- liftIO $ Build.getDerivation store drvPath
  derivation <- case derivationMaybe of
    Just drv -> pure drv
    Nothing -> panic $ "Could not retrieve derivation " <> show drvPath <> " from local store or binary caches."
  nixBuildResult <- liftIO $ buildDerivation store drvPath derivation (extraPaths <$ guard (not materialize))
  katipAddContext (sl "result" (show nixBuildResult :: Text)) $
    logLocM DebugS "Build result"
  buildResult <- liftIO $ enrichResult store derivation nixBuildResult
  yield $ Event.BuildResult buildResult

-- TODO: case distinction on BuildStatus enumeration
enrichResult :: Ptr (Ref NixStore) -> ForeignPtr Derivation -> Build.BuildResult -> IO Event.BuildResult.BuildResult
enrichResult _ _ result@Build.BuildResult {isSuccess = False} =
  pure $
    Event.BuildResult.BuildFailure {errorMessage = Build.errorMessage result}
enrichResult store derivation _ = do
  drvOuts <- getDerivationOutputs derivation
  outputInfos <- for drvOuts $ \drvOut -> do
    vpi <- queryPathInfo (coerceStore store) (derivationOutputPath drvOut)
    hash_ <- validPathInfoNarHash vpi
    let size = validPathInfoNarSize vpi
    pure
      Event.BuildResult.OutputInfo
        { name = derivationOutputName drvOut,
          path = derivationOutputPath drvOut,
          hash = hash_,
          size = size
        }
  pure $ Event.BuildResult.BuildSuccess outputInfos

-- TODO factor out cnix library and avoid unsafeCoerce https://github.com/hercules-ci/hercules-ci-agent/issues/223
coerceStore :: Ptr (Ref NixStore) -> Store
coerceStore = unsafeCoerce
