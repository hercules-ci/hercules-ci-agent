{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Build where

import Conduit
import Data.Conduit.Katip.Orphans ()
import Foreign (ForeignPtr)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Command.Build
import Hercules.Agent.WorkerProtocol.Event (Event)
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.BuildResult as Event.BuildResult
import Hercules.CNix
  ( DerivationOutput (derivationOutputName, derivationOutputPath),
    NixStore,
    Ref,
    getDerivationOutputs,
  )
import qualified Hercules.CNix as CNix
import Hercules.CNix.Store (Store, queryPathInfo, validPathInfoNarHash, validPathInfoNarSize)
import Hercules.CNix.Store.Context (Derivation)
import Katip
import Protolude hiding (yield)
import Unsafe.Coerce

runBuild :: (MonadIO m, KatipContext m) => Store -> Command.Build.Build -> ConduitT i Event m ()
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
      CNix.logInfo "unable to retrieve dependency; attempting fallback to local build"
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
enrichResult :: Store -> ForeignPtr Derivation -> Build.BuildResult -> IO Event.BuildResult.BuildResult
enrichResult _ _ result@Build.BuildResult {isSuccess = False} =
  pure $
    Event.BuildResult.BuildFailure {errorMessage = Build.errorMessage result}
enrichResult store derivation _ = do
  drvOuts <- getDerivationOutputs derivation
  outputInfos <- for drvOuts $ \drvOut -> do
    vpi <- queryPathInfo store (derivationOutputPath drvOut)
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
