{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Build where

import Conduit
import Data.Conduit.Katip.Orphans ()
import Hercules.Agent.Store (toDrvInfo)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import Hercules.Agent.Worker.Build.Prefetched qualified as Build
import Hercules.Agent.WorkerProtocol.Command.Build qualified as Command.Build
import Hercules.Agent.WorkerProtocol.Event (Event)
import Hercules.Agent.WorkerProtocol.Event qualified as Event
import Hercules.Agent.WorkerProtocol.Event.BuildResult qualified as Event.BuildResult
import Hercules.CNix
  ( getDerivationOutputs,
  )
import Hercules.CNix qualified as CNix
import Hercules.CNix.Store (Store)
import Katip
import Protolude hiding (yield)

runBuild :: (KatipContext m) => Store -> Command.Build.Build -> ConduitT i Event m ()
runBuild store build = katipAddContext (sl "derivationPath" build.drvPath) do
  logLocM DebugS "runBuild"
  let extraPaths = Command.Build.inputDerivationOutputPaths build
      drvPath = encodeUtf8 $ Command.Build.drvPath build
  drvStorePath <- liftIO $ CNix.parseStorePath store drvPath
  x <- for extraPaths $ \input -> liftIO $ do
    storePath <- CNix.parseStorePath store input
    CNix.addTemporaryRoot store storePath
    try $ CNix.ensurePath store storePath
  materialize0 <- case sequenceA x of
    Right _ ->
      -- no error, proceed with requested materialization setting
      pure $ Command.Build.materializeDerivation build
    Left (e :: SomeException) -> liftIO do
      CNix.logInfo $ "while retrieving dependencies: " <> toS (displayException e)
      CNix.logInfo "unable to retrieve dependency; attempting fallback to local build"
      pure True
  drvName <- liftIO $ CNix.getDerivationNameFromPath drvStorePath
  derivationMaybe <- liftIO $ Build.getDerivation store drvStorePath
  derivation <- case derivationMaybe of
    Just drv -> pure drv
    Nothing -> panic $ "Could not retrieve derivation " <> show drvStorePath <> " from local store or binary caches."
  drvPlatform <- liftIO $ CNix.getDerivationPlatform derivation
  let mayNeedRemoteBuild = drvPlatform `elem` Command.Build.materializePlatforms build
      materialize = materialize0 || mayNeedRemoteBuild
  nixBuildResult <- liftIO $ buildDerivation store drvStorePath derivation (extraPaths <$ guard (not materialize))
  katipAddContext (sl "result" (show nixBuildResult :: Text)) $
    logLocM DebugS "Build result"
  buildResult <- liftIO $ enrichResult store drvName derivation nixBuildResult
  yield $ Event.BuildResult buildResult

-- TODO: case distinction on BuildStatus enumeration
enrichResult :: Store -> ByteString -> CNix.Derivation -> Build.BuildResult -> IO Event.BuildResult.BuildResult
enrichResult _ _ _ result@Build.BuildResult {isSuccess = False} =
  pure $
    Event.BuildResult.BuildFailure {errorMessage = Build.errorMessage result}
enrichResult store drvName derivation _ = do
  drvOuts <- getDerivationOutputs store drvName derivation
  outputInfos <- for drvOuts $ toDrvInfo store
  pure $ Event.BuildResult.BuildSuccess outputInfos
