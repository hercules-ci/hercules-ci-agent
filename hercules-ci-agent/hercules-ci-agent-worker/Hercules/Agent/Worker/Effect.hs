{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Effect where

import CNix
import CNix.Internal.Context (Derivation)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import GHC.ForeignPtr (ForeignPtr)
import Hercules.Agent.Sensitive (Sensitive (Sensitive), reveal, revealContainer)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import Hercules.Agent.Worker.Effect.Container as Container
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Command.Effect
import qualified Hercules.Formats.Secret as Formats.Secret
import Katip
import Protolude
import System.Directory (createDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

runEffect :: (MonadIO m, KatipContext m) => Ptr (Ref NixStore) -> Command.Effect.Effect -> m ExitCode
runEffect store command = do
  derivation <- prepareDerivation store command
  drvBuilder <- liftIO $ getDerivationBuilder derivation
  drvArgs <- liftIO $ getDerivationArguments derivation
  drvEnv <- liftIO $ getDerivationEnv derivation
  drvSecretsMap <- parseDrvSecretsMap drvEnv
  dir <- liftIO getCurrentDirectory
  let mkDir d = let newDir = dir </> d in toS newDir <$ liftIO (createDirectory newDir)
  buildDir <- mkDir "build"
  etcDir <- mkDir "etc"
  secretsDir <- mkDir "secrets"
  let extraSecrets :: Map Text (Sensitive Formats.Secret.Secret)
      extraSecrets = M.singleton "hercules-ci" $ do
        token <- Command.Effect.token command
        pure $
          Formats.Secret.Secret
            { data_ = M.singleton "token" $ A.String token
            }
  writeSecrets (Command.Effect.secretsPath command) drvSecretsMap extraSecrets (toS secretsDir)
  liftIO $ do
    -- Nix sandbox sets tmp to buildTopDir
    -- Nix sandbox reference: https://github.com/NixOS/nix/blob/24e07c428f21f28df2a41a7a9851d5867f34753a/src/libstore/build.cc#L2545
    --
    -- TODO: what if we have structuredAttrs?
    -- TODO: implement passAsFile?
    let overridableEnv, onlyImpureOverridableEnv, fixedEnv :: Map Text Text
        overridableEnv =
          M.fromList
            [ ("PATH", "/path-not-set"),
              ("HOME", "/homeless-shelter"),
              ("NIX_STORE", "/nix/store"), -- TODO store.storeDir
              ("NIX_BUILD_CORES", "1"), -- not great
              ("NIX_REMOTE", "daemon"),
              ("IN_HERCULES_CI_EFFECT", "true"),
              ("HERCULES_CI_API_BASE_URL", Command.Effect.apiBaseURL command),
              ("HERCULES_CI_SECRETS_JSON", "/secrets/secrets.json")
            ]
        -- NB: this is lossy. Consider using ByteString-based process functions
        drvEnv' = drvEnv & M.mapKeys (decodeUtf8With lenientDecode) & fmap (decodeUtf8With lenientDecode)
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
    Container.run
      Container.Config
        { extraBindMounts =
            [ BindMount {pathInContainer = "/build", pathInHost = buildDir, readOnly = False},
              BindMount {pathInContainer = "/etc", pathInHost = etcDir, readOnly = False},
              BindMount {pathInContainer = "/secrets", pathInHost = secretsDir, readOnly = True},
              BindMount {pathInContainer = "/etc/resolv.conf", pathInHost = "/etc/resolv.conf", readOnly = True},
              BindMount {pathInContainer = "/nix/var/nix/daemon-socket/socket", pathInHost = "/nix/var/nix/daemon-socket/socket", readOnly = True}
            ],
          executable = decodeUtf8With lenientDecode drvBuilder,
          arguments = map (decodeUtf8With lenientDecode) drvArgs,
          environment = overridableEnv // drvEnv' // onlyImpureOverridableEnv // impureEnvVars // fixedEnv,
          workingDirectory = "/build",
          hostname = "hercules-ci",
          rootReadOnly = False
        }

parseDrvSecretsMap :: MonadIO m => Map ByteString ByteString -> m (Map Text Text)
parseDrvSecretsMap drvEnv =
  case drvEnv & M.lookup "secretsMap" of
    Nothing -> pure mempty
    Just secretsMapText -> case A.eitherDecode (BL.fromStrict secretsMapText) of
      Left _ -> throwIO $ FatalError "Could not parse secretsMap variable in derivation. It must be a JSON dictionary of strings referencing agent secret names."
      Right r -> pure r

type SecretData = Sensitive (Map Text A.Value)

writeSecrets :: (MonadIO m, KatipContext m) => FilePath -> Map Text Text -> Map Text (Sensitive Formats.Secret.Secret) -> FilePath -> m ()
writeSecrets sourceFile secretsMap extraSecrets destinationDirectory = write . fmap reveal . addExtra =<< gather
  where
    addExtra = flip M.union extraSecrets
    write = liftIO . BS.writeFile (destinationDirectory </> "secrets.json") . BL.toStrict . A.encode
    gather =
      if null secretsMap
        then pure mempty
        else do
          secretsBytes <- liftIO $ BS.readFile sourceFile
          r <- case A.eitherDecode $ BL.fromStrict secretsBytes of
            Left e -> do
              logLocM ErrorS $ "Could not parse secrets file " <> logStr sourceFile <> ": " <> logStr e
              throwIO $ FatalError "Could not parse secrets file as configured on agent."
            Right r -> pure (Sensitive r)
          liftIO $ createDirectoryIfMissing True destinationDirectory
          secretsMap & M.traverseWithKey \destinationName (secretName :: Text) -> do
            case revealContainer (r <&> M.lookup secretName) of
              Nothing ->
                liftIO $
                  throwIO $
                    FatalError $
                      "Secret " <> secretName <> " does not exist, so we can't find a secret for " <> destinationName <> ". Please make sure that the secret name matches a secret on your agents."
              Just ssecret ->
                pure do
                  secret <- ssecret
                  -- Currently this is `id` but we might want to fork the
                  -- format here or omit some fields.
                  pure $
                    Formats.Secret.Secret
                      { data_ = Formats.Secret.data_ secret
                      }

prepareDerivation :: MonadIO m => Ptr (Ref NixStore) -> Command.Effect.Effect -> m (ForeignPtr Derivation)
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
  sources <- liftIO $ getDerivationSources derivation
  for_ sources \src -> do
    liftIO $ CNix.ensurePath store src
  pure derivation
