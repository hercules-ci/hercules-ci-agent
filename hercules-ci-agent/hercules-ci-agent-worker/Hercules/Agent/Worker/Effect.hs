{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Effect where

import Control.Monad.Catch (MonadThrow)
import GHC.ForeignPtr (ForeignPtr)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Command.Effect
import Hercules.CNix (Store)
import qualified Hercules.CNix as CNix
import Hercules.CNix.Store.Context (Derivation)
import qualified Hercules.Effect as Effect
import Katip (KatipContext)
import Protolude
import UnliftIO.Directory (getCurrentDirectory)

runEffect :: (MonadIO m, KatipContext m, MonadThrow m) => Store -> Command.Effect.Effect -> m ExitCode
runEffect store command = do
  derivation <- prepareDerivation store command
  dir <- getCurrentDirectory
  Effect.runEffect derivation (Command.Effect.token command) (Command.Effect.secretsPath command) (Command.Effect.apiBaseURL command) dir

prepareDerivation :: MonadIO m => Store -> Command.Effect.Effect -> m (ForeignPtr Derivation)
prepareDerivation store command = do
  let extraPaths = Command.Effect.inputDerivationOutputPaths command
      drvPath = encodeUtf8 $ Command.Effect.drvPath command
      ensureDeps = for_ extraPaths $ \input ->
        liftIO $ CNix.ensurePath store input
  liftIO $ do
    ensureDeps `catch` \e -> do
      CNix.logInfo $ "while retrieving dependencies: " <> toS (displayException (e :: SomeException))
      CNix.logInfo "unable to retrieve dependency; attempting fallback to local build"
      CNix.ensurePath store drvPath
      derivation <- CNix.getDerivation store drvPath
      depDrvPaths <- CNix.getDerivationInputs derivation
      for_ depDrvPaths \(depDrv, _outputs) -> do
        depDerivation <- CNix.getDerivation store depDrv
        _nixBuildResult <- liftIO $ buildDerivation store depDrv depDerivation mempty
        pass
  derivation <-
    liftIO (Build.getDerivation store drvPath) >>= \case
      Just drv -> pure drv
      Nothing -> panic $ "Could not retrieve derivation " <> show drvPath <> " from local store or binary caches."
  sources <- liftIO $ CNix.getDerivationSources derivation
  for_ sources \src -> do
    liftIO $ CNix.ensurePath store src
  pure derivation
