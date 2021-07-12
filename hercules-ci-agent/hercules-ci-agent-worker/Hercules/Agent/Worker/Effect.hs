{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Effect where

import Control.Monad.Catch (MonadThrow)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Command.Effect
import Hercules.CNix (Store)
import qualified Hercules.CNix as CNix
import Hercules.CNix.Store (Derivation)
import Hercules.Effect (RunEffectParams (..))
import qualified Hercules.Effect as Effect
import Katip (KatipContext)
import Protolude
import UnliftIO.Directory (getCurrentDirectory)

runEffect :: (MonadIO m, KatipContext m, MonadThrow m) => Store -> Command.Effect.Effect -> m ExitCode
runEffect store command = do
  derivation <- prepareDerivation store command
  dir <- getCurrentDirectory
  Effect.runEffect
    RunEffectParams
      { runEffectDerivation = derivation,
        runEffectToken = Just $ Command.Effect.token command,
        runEffectSecretsConfigPath = Just $ Command.Effect.secretsPath command,
        runEffectApiBaseURL = Command.Effect.apiBaseURL command,
        runEffectDir = dir,
        runEffectProjectId = Just $ Command.Effect.projectId command,
        runEffectProjectPath = Just $ Command.Effect.projectPath command
      }

prepareDerivation :: MonadIO m => Store -> Command.Effect.Effect -> m Derivation
prepareDerivation store command = do
  let extraPaths = Command.Effect.inputDerivationOutputPaths command
      drvPath = encodeUtf8 $ Command.Effect.drvPath command
      ensureDeps = for_ extraPaths $ \input -> liftIO $ do
        CNix.ensurePath store =<< CNix.parseStorePath store input
  drvStorePath <- liftIO $ CNix.parseStorePath store drvPath
  liftIO $ do
    ensureDeps `catch` \e -> do
      CNix.logInfo $ "while retrieving dependencies: " <> toS (displayException (e :: SomeException))
      CNix.logInfo "unable to retrieve dependency; attempting fallback to local build"
      CNix.ensurePath store drvStorePath
      derivation <- CNix.getDerivation store drvStorePath
      depDrvPaths <- CNix.getDerivationInputs store derivation
      for_ depDrvPaths \(depDrv, _outputs) -> do
        depDerivation <- CNix.getDerivation store depDrv
        _nixBuildResult <- liftIO $ buildDerivation store depDrv depDerivation mempty
        pass
  derivation <-
    liftIO (Build.getDerivation store drvStorePath) >>= \case
      Just drv -> pure drv
      Nothing -> panic $ "Could not retrieve derivation " <> show drvPath <> " from local store or binary caches."
  sources <- liftIO $ CNix.getDerivationSources store derivation
  for_ sources \src -> do
    liftIO $ CNix.ensurePath store src
  pure derivation
