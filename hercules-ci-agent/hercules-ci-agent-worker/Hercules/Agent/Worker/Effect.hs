module Hercules.Agent.Worker.Effect where

import CNix
import CNix.Internal.Context (Derivation)
import qualified Data.Map as M
import GHC.ForeignPtr (ForeignPtr)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import Hercules.Agent.Worker.Effect.Container as Container
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Command.Effect
import Protolude
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)

runEffect :: MonadIO m => Ptr (Ref NixStore) -> Command.Effect.Effect -> m ExitCode
runEffect store command = do
  derivation <- prepareDerivation store command
  drvBuilder <- liftIO $ getDerivationBuilder derivation
  drvArgs <- liftIO $ getDerivationArguments derivation
  drvEnv <- liftIO $ getDerivationEnv derivation
  liftIO $ withTempDirectory "/home/user/h/hercules-ci-agent/.temp" "effect-work" $ \dir -> do
    let mkDir d = let newDir = dir </> d in toS newDir <$ liftIO (createDirectory newDir)
    buildDir <- mkDir "build"
    -- Nix sandbox sets tmp to buildTopDir

    -- Nix sandbox reference: https://github.com/NixOS/nix/blob/24e07c428f21f28df2a41a7a9851d5867f34753a/src/libstore/build.cc#L2545
    --
    -- TODO: what if we have structuredAttrs?
    -- TODO: implement passAsFile?
    let overridableEnv =
          M.fromList
            [ ("PATH", "/path-not-set"),
              ("HOME", "/homeless-shelter"),
              ("NIX_STORE", "/nix/store"), -- TODO store.storeDir
              ("NIX_BUILD_CORES", "1"), -- not great
              ("IN_HERCULES_CI_EFFECT", "true")
            ]
        impureEnvVars = mempty -- TODO
        fixedEnv =
          M.fromList
            [ ("NIX_LOG_FD", "2"),
              ("TERM", "xterm-256color")
            ]
        onlyImpureOverridableEnv =
          M.fromList
            [ ("NIX_BUILD_TOP", "/build"),
              ("TMPDIR", "/build"),
              ("TEMPDIR", "/build"),
              ("TMP", "/build"),
              ("TEMP", "/build")
            ]
        (//) :: Ord k => Map k a -> Map k a -> Map k a
        (//) = flip M.union
    exitCode <- Container.run Container.Config
      { extraBindMounts =
          [ BindMount {source = "/build", destination = buildDir, readOnly = False} --,
                -- BindMount { source = buildDir, destination = "/tmp", readOnly = False }
          ],
        executable = toS drvBuilder,
        arguments = map toS drvArgs,
        environment =
          (overridableEnv // drvEnv // onlyImpureOverridableEnv // impureEnvVars // fixedEnv)
            & M.mapKeys toSL
            & M.map toSL,
        workingDirectory = "/build"
      }
    pure exitCode

prepareDerivation :: MonadIO m => Ptr (Ref NixStore) -> Command.Effect.Effect -> m (ForeignPtr Derivation)
prepareDerivation store command = do
  let extraPaths = Command.Effect.inputDerivationOutputPaths command
      drvPath = toS $ Command.Effect.drvPath command
  for_ extraPaths $ \input ->
    liftIO $ CNix.ensurePath store input
  derivationMaybe <- liftIO $ Build.getDerivation store drvPath
  case derivationMaybe of
    Just drv -> pure drv
    Nothing -> panic $ "Could not retrieve derivation " <> show drvPath <> " from local store or binary caches."
