{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Effect where

import Control.Monad.Catch (MonadThrow)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import Hercules.Agent.Worker.Build.Prefetched qualified as Build
import Hercules.Agent.WorkerProtocol.Command.Effect qualified as Command.Effect
import Hercules.Agent.WorkerProtocol.ViaJSON (fromViaJSON)
import Hercules.CNix (Store)
import Hercules.CNix qualified as CNix
import Hercules.CNix.Store (Derivation)
import Hercules.Effect (RunEffectParams (..))
import Hercules.Effect qualified as Effect
import Katip (KatipContext)
import Protolude
import UnliftIO.Directory (getCurrentDirectory)

runEffect :: (KatipContext m, MonadThrow m) => [(Text, Text)] -> Store -> Command.Effect.Effect -> m ExitCode
runEffect extraNixOptions store command = do
  derivation <- prepareDerivation store command
  dir <- getCurrentDirectory
  Effect.runEffect
    RunEffectParams
      { runEffectDerivation = derivation,
        runEffectToken = Just $ Command.Effect.token command,
        runEffectSecretsConfigPath = Just $ Command.Effect.secretsPath command,
        runEffectServerSecrets = Command.Effect.serverSecrets command <&> fromViaJSON,
        runEffectApiBaseURL = Command.Effect.apiBaseURL command,
        runEffectDir = dir,
        runEffectProjectId = Just $ Command.Effect.projectId command,
        runEffectProjectPath = Just $ Command.Effect.projectPath command,
        runEffectSecretContext = Just $ Command.Effect.secretContext command,
        runEffectUseNixDaemonProxy = True,
        runEffectExtraNixOptions = extraNixOptions,
        runEffectFriendly = False
      }

prepareDerivation :: (MonadIO m) => Store -> Command.Effect.Effect -> m Derivation
prepareDerivation store command = do
  let extraPaths = Command.Effect.inputDerivationOutputPaths command
      drvPath = encodeUtf8 $ Command.Effect.drvPath command
      ensureDeps = for_ extraPaths $ \input -> liftIO $ do
        p <- CNix.parseStorePath store input
        CNix.addTemporaryRoot store p
        CNix.ensurePath store p
  drvStorePath <- liftIO $ CNix.parseStorePath store drvPath
  liftIO $ do
    ensureDeps `catch` \e -> do
      CNix.logInfo $ "while retrieving dependencies: " <> toS (displayException (e :: SomeException))
      CNix.logInfo "unable to retrieve dependency; attempting fallback to local build"
      -- TODO read directly from cache?
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
  for_ sources \src -> liftIO do
    CNix.addTemporaryRoot store src
    CNix.ensurePath store src
  pure derivation
