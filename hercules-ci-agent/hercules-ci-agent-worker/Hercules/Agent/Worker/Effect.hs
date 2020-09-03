{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Effect where

import CNix
import CNix.Internal.Context (Derivation)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Map as M
import GHC.ForeignPtr (ForeignPtr)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import Hercules.Agent.Worker.Effect.Container as Container
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Command.Effect
import qualified Hercules.Formats.Secret as Secret
import Katip
import Protolude hiding (sourceFile)
import System.Directory (createDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

runEffect :: (MonadIO m, KatipContext m) => Ptr (Ref NixStore) -> Command.Effect.Effect -> m ExitCode
runEffect store command = do
  derivation <- prepareDerivation store command
  drvBuilder <- liftIO $ getDerivationBuilder derivation
  drvArgs <- liftIO $ getDerivationArguments derivation
  drvEnv <- liftIO $ getDerivationEnv derivation
  drvSecretsMap <- parseDrvSecretsMap drvEnv
  dir <- liftIO $ getCurrentDirectory
  let mkDir d = let newDir = dir </> d in toS newDir <$ liftIO (createDirectory newDir)
  buildDir <- mkDir "build"
  secretsDir <- mkDir "secrets"
  writeSecrets (Command.Effect.secretsPath command) drvSecretsMap (toS secretsDir)
  liftIO $ do
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
          [ BindMount {pathInContainer = "/build", pathInHost = buildDir, readOnly = False},
            BindMount {pathInContainer = "/secrets", pathInHost = secretsDir, readOnly = True}
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

parseDrvSecretsMap :: MonadIO m => Map ByteString ByteString -> m (Map Text Text)
parseDrvSecretsMap drvEnv =
  case drvEnv & M.lookup "secretsMap" of
    Nothing -> pure mempty
    Just secretsMapText -> case A.eitherDecode (toSL $ secretsMapText) of
      Left _ -> throwIO $ FatalError "Could not parse secretsMap variable in derivation. It must be a JSON dictionary of strings referencing agent secret names."
      Right r -> pure r

writeSecrets :: (MonadIO m, KatipContext m) => FilePath -> Map Text Text -> FilePath -> m ()
writeSecrets _ secretsMap destinationDirectory
  | null secretsMap =
    -- avoid handling secrets unless necessary
    liftIO $ BS.writeFile (destinationDirectory </> "secrets.json") "{}"
writeSecrets sourceFile secretsMap destinationDirectory = do
  secretsBytes <- liftIO $ BS.readFile sourceFile
  r <- case A.eitherDecode $ toS secretsBytes of
    Left e -> do
      logLocM ErrorS $ "Could not parse secrets file " <> logStr sourceFile <> ": " <> logStr e
      throwIO $ FatalError $ "Could not parse secrets file as configured on agent."
    Right r -> pure r
  liftIO $ createDirectoryIfMissing True destinationDirectory
  out <- secretsMap & M.traverseWithKey \destinationName (secretName :: Text) ->
    case M.lookup secretName r of
      Nothing ->
        liftIO $ throwIO $ FatalError $
          "Secret " <> secretName <> " does not exist, so we can't find a secret for " <> destinationName <> ". Please make sure that the secret name matches a secret on your agents."
      Just secret -> pure (Secret.data_ secret)
  liftIO $ BS.writeFile (destinationDirectory </> "secrets.json") (toS $ A.encode out)

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
