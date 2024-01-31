{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Effect where

import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled))
import Control.Exception.Safe (isAsyncException)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as AK
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Text qualified as T
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent (GitToken (..), SecretRef (GitToken, SimpleSecret), SimpleSecret (MkSimpleSecret))
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent qualified
import Hercules.API.Id (Id, idText)
import Hercules.Agent.Sensitive (Sensitive (Sensitive, reveal), revealContainer)
import Hercules.Agent.WorkerProcess qualified as WorkerProcess
import Hercules.CNix (Derivation)
import Hercules.CNix.Store (getDerivationArguments, getDerivationBuilder, getDerivationEnv)
import Hercules.Effect.Container (BindMount (BindMount))
import Hercules.Effect.Container qualified as Container
import Hercules.Error (escalateAs)
import Hercules.Formats.Mountable (Mountable)
import Hercules.Formats.Mountable qualified as Mountable
import Hercules.Formats.Secret qualified as Formats.Secret
import Hercules.Secrets (SecretContext, evalCondition, evalConditionTrace)
import Katip (KatipContext, Severity (..), logLocM, logStr)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), bind, listen, socket, withFdSocket)
import Protolude
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Posix (dup, fdToHandle)
import System.Posix.Signals (killProcess, signalProcess)
import System.Process (ProcessHandle)
import System.Process.Internals qualified as Process.Internal
import UnliftIO.Directory (createDirectory, createDirectoryIfMissing)
import UnliftIO.Process (withCreateProcess)
import UnliftIO.Process qualified as Process

parseDrvMountsMap :: Map ByteString ByteString -> Either Text (Map Text Text)
parseDrvMountsMap drvEnv =
  case "__hci_mounts" `M.lookup` drvEnv of
    Nothing -> pure mempty
    Just mountsMapText -> case A.eitherDecode (BL.fromStrict mountsMapText) of
      Left e -> Left $ "Could not parse __hci_mounts variable in derivation. Error: " <> toS e
      Right r -> pure r

parseDrvSecretsMap :: Map ByteString ByteString -> Either Text (Map Text SecretRef)
parseDrvSecretsMap drvEnv =
  case (,) "secretsToUse" <$> M.lookup "secretsToUse" drvEnv
    <|> (,) "secretsMap" <$> M.lookup "secretsMap" drvEnv of
    Nothing -> pure mempty
    Just (attrName, secretsMapText) -> case A.eitherDecode (BL.fromStrict secretsMapText) of
      Left e -> Left $ "Could not parse " <> attrName <> " variable in derivation. Error: " <> toS e
      Right r -> parseSecretRefs attrName r

parseSecretRefs :: Text -> A.Object -> Either Text (Map Text SecretRef)
parseSecretRefs attrName obj =
  AK.toMapText obj & M.traverseWithKey \k v -> parseSecretRef (attrName <> "." <> k) v

parseSecretRef :: Text -> A.Value -> Either Text SecretRef
parseSecretRef _ (A.String s) = pure (SimpleSecret $ MkSimpleSecret s)
parseSecretRef attrName (A.Object o) =
  case AK.lookup "type" o of
    Just (A.String s) ->
      case s of
        "GitToken" -> pure (GitToken $ MkGitToken {})
        _ -> Left $ "Could not parse secret reference " <> attrName <> ", because the type is unknown; must be \"GitToken\"."
    Just _ -> Left $ "Could not parse secret reference " <> attrName <> ", because the type attribute is not a string."
    Nothing -> Left $ "Could not parse secret reference " <> attrName <> ", because it does not have a type attribute."
parseSecretRef attrName _ = Left $ "Could not parse secret reference " <> attrName <> ", because it is neither a string nor an attribute set."

-- | Write secrets to file based on secretsMap value
writeSecrets ::
  (KatipContext m) =>
  -- | Whether we're in a friendly context, such as the CLI.
  Bool ->
  Maybe SecretContext ->
  -- | Optional source file
  Maybe FilePath ->
  -- | Declared secrets from the effect derivation
  Map Text SecretRef ->
  -- | Local secrets
  Map Text (Sensitive Formats.Secret.Secret) ->
  -- | Server secrets
  Map Text (Sensitive (Map Text A.Value)) ->
  FilePath ->
  m ()
writeSecrets friendly ctxMaybe sourceFileMaybe secretsMap extraSecrets serverSecrets destinationDirectory = write . fmap reveal . addExtra =<< gather
  where
    addExtra = flip M.union extraSecrets
    write = liftIO . BS.writeFile (destinationDirectory </> "secrets.json") . BL.toStrict . A.encode
    gather =
      if null secretsMap
        then pure mempty
        else do
          allSecrets <-
            sourceFileMaybe & maybe (purer mempty) \sourceFile -> do
              secretsBytes <- liftIO $ BS.readFile sourceFile
              case A.eitherDecode $ BL.fromStrict secretsBytes of
                Left e -> do
                  logLocM ErrorS $ "Could not parse secrets file " <> logStr sourceFile <> ": " <> logStr e
                  throwIO $ FatalError "Could not parse secrets file as configured on agent."
                Right r -> pure (Sensitive r)

          createDirectoryIfMissing True destinationDirectory

          secretsMap & M.traverseWithKey \destinationName secretRef ->
            case secretRef of
              GitToken {} -> do
                case M.lookup destinationName serverSecrets of
                  Just x -> pure (x <&> \data_ -> Formats.Secret.Secret {data_ = data_, condition = Just (Formats.Secret.And [])})
                  Nothing ->
                    liftIO . throwIO . FatalError $
                      "A value for secret " <> destinationName <> " was not provided. This may be a bug."
              SimpleSecret (MkSimpleSecret {name = secretName}) -> do
                let gotoFail name =
                      liftIO . throwIO . FatalError $
                        "Secret " <> name <> " does not exist or access was denied, so we can't get a secret for " <> destinationName <> ". Please make sure that the secret name matches a secret on your agents and make sure that its condition applies."

                case revealContainer (allSecrets <&> M.lookup secretName) of
                  Nothing -> gotoFail secretName
                  Just ssecret -> do
                    let condMaybe = reveal (Formats.Secret.condition <$> ssecret)
                        r = do
                          secret <- ssecret
                          pure $
                            Formats.Secret.Secret
                              { data_ = Formats.Secret.data_ secret,
                                -- Hide the condition
                                condition = Nothing
                              }
                    case (friendly, condMaybe) of
                      (True, Nothing) -> do
                        putErrText $ "The secret " <> show secretName <> " does not contain the `condition` field, which is required on hercules-ci-agent >= 0.9."
                        pure r
                      (True, Just cond) | Just ctx <- ctxMaybe ->
                        case evalConditionTrace ctx cond of
                          (_, True) -> pure r
                          (trace_, _) -> do
                            putErrText $ "Could not grant access to secret " <> show secretName <> "."
                            for_ trace_ \ln -> putErrText $ "  " <> ln
                            liftIO . throwIO . FatalError $ "Could not grant access to secret " <> show secretName <> ". See trace in preceding log."
                      (True, Just _) | otherwise -> do
                        -- This is only ok in friendly mode (hci)
                        putErrText "WARNING: not performing secrets access control. The secret.condition field won't be checked."
                        pure r
                      (False, Nothing) -> gotoFail secretName
                      (False, Just cond) ->
                        if evalCondition (fromMaybe (panic "SecretContext is required") ctxMaybe) cond then pure r else gotoFail secretName

data RunEffectParams = RunEffectParams
  { runEffectDerivation :: Derivation,
    runEffectToken :: Maybe (Sensitive Text),
    runEffectSecretsConfigPath :: Maybe FilePath,
    runEffectSecretContext :: Maybe SecretContext,
    runEffectServerSecrets :: Sensitive (Map Text (Map Text A.Value)),
    runEffectConfiguredMountables :: Sensitive (Map Text Mountable),
    runEffectApiBaseURL :: Text,
    runEffectDir :: FilePath,
    runEffectProjectId :: Maybe (Id "project"),
    runEffectProjectPath :: Maybe Text,
    runEffectUseNixDaemonProxy :: Bool,
    runEffectExtraNixOptions :: [(Text, Text)],
    -- | Whether we can relax security in favor of usability; 'True' in @hci effect run@. 'False' in agent.
    runEffectFriendly :: Bool
  }

(=:) :: k -> a -> Map k a
(=:) = M.singleton

runEffect :: (MonadThrow m, KatipContext m) => RunEffectParams -> m ExitCode
runEffect p@RunEffectParams {runEffectDerivation = derivation, runEffectSecretsConfigPath = secretsPath, runEffectApiBaseURL = apiBaseURL, runEffectDir = dir, runEffectServerSecrets = serverSecrets} = do
  drvBuilder <- liftIO $ getDerivationBuilder derivation
  drvArgs <- liftIO $ getDerivationArguments derivation
  drvEnv <- liftIO $ getDerivationEnv derivation
  drvSecretsMap <- escalateAs FatalError $ parseDrvSecretsMap drvEnv
  drvMountsMap <- escalateAs FatalError $ parseDrvMountsMap drvEnv
  let mkDir d = let newDir = dir </> d in toS newDir <$ createDirectory newDir
  buildDir <- mkDir "build"
  etcDir <- mkDir "etc"
  secretsDir <- mkDir "secrets"
  containerDir <- mkDir "container-state"
  let extraSecrets =
        runEffectToken p
          & maybe
            mempty
            ( \token ->
                "hercules-ci" =: do
                  tok <- token
                  pure $
                    Formats.Secret.Secret
                      { data_ = M.singleton "token" $ A.String tok,
                        condition = Nothing
                      }
            )
  writeSecrets (runEffectFriendly p) (runEffectSecretContext p) secretsPath drvSecretsMap extraSecrets (revealContainer serverSecrets) (toS secretsDir)
  liftIO $ do
    -- Nix sandbox sets tmp to buildTopDir
    -- Nix sandbox reference: https://github.com/NixOS/nix/blob/24e07c428f21f28df2a41a7a9851d5867f34753a/src/libstore/build.cc#L2545
    --
    -- TODO: what if we have structuredAttrs?
    -- TODO: implement passAsFile?
    let overridableEnv, onlyImpureOverridableEnv, fixedEnv :: Map Text Text
        overridableEnv =
          M.fromList $
            [ ("PATH", "/path-not-set"),
              ("HOME", "/homeless-shelter"),
              ("NIX_STORE", "/nix/store"), -- TODO store.storeDir
              ("NIX_BUILD_CORES", "1"), -- not great
              ("NIX_REMOTE", "daemon"),
              ("IN_HERCULES_CI_EFFECT", "true"),
              ("HERCULES_CI_API_BASE_URL", apiBaseURL),
              ("HERCULES_CI_SECRETS_JSON", "/secrets/secrets.json")
            ]
              <> [("HERCULES_CI_PROJECT_ID", idText x) | x <- toList $ runEffectProjectId p]
              <> [("HERCULES_CI_PROJECT_PATH", x) | x <- toList $ runEffectProjectPath p]

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
        (//) :: (Ord k) => Map k a -> Map k a -> Map k a
        (//) = flip M.union
    let (withNixDaemonProxyPerhaps, forwardedSocketPath) =
          if runEffectUseNixDaemonProxy p
            then
              let socketPath = dir </> "nix-daemon-socket"
               in (withNixDaemonProxy (runEffectExtraNixOptions p) socketPath, socketPath)
            else (identity, "/nix/var/nix/daemon-socket/socket")
    extraBindMounts_ <- checkMounts (reveal $ runEffectConfiguredMountables p) (runEffectSecretContext p) drvMountsMap
    -- We've validated that the paths are pretty much canonical; otherwise this check would be insufficient.
    let isExtraBind path = extraBindMounts_ & any (\m -> Container.pathInContainer m == path)
    withNixDaemonProxyPerhaps $
      Container.run
        containerDir
        Container.Config
          { extraBindMounts =
              [ BindMount {pathInContainer = "/build", pathInHost = buildDir, readOnly = False},
                BindMount {pathInContainer = "/secrets", pathInHost = secretsDir, readOnly = True},
                BindMount {pathInContainer = "/nix/var/nix/daemon-socket/socket", pathInHost = toS forwardedSocketPath, readOnly = True}
              ]
                ++ [ BindMount {pathInContainer = "/etc", pathInHost = etcDir, readOnly = False}
                     | not (isExtraBind "/etc")
                   ]
                ++ [
                     -- TODO: does this apply to crun?
                     -- we cannot bind mount this read-only because of https://github.com/opencontainers/runc/issues/1523
                     BindMount {pathInContainer = "/etc/resolv.conf", pathInHost = "/etc/resolv.conf", readOnly = False}
                     | not (isExtraBind "/etc") && not (isExtraBind "/etc/resolv.conf")
                   ]
                ++ extraBindMounts_,
            executable = decodeUtf8With lenientDecode drvBuilder,
            arguments = map (decodeUtf8With lenientDecode) drvArgs,
            environment = overridableEnv // drvEnv' // onlyImpureOverridableEnv // impureEnvVars // fixedEnv,
            workingDirectory = "/build",
            hostname = "hercules-ci",
            rootReadOnly = False
          }

checkMounts :: Map Text Mountable -> Maybe SecretContext -> Map Text Text -> IO [BindMount]
checkMounts configuredMnts secretContext drvMounts = do
  concat <$> forM (M.toList drvMounts) \(mntPath, mntName) -> do
    let -- Intentionally generic message, as misconfigurations on the agent are somewhat sensitive.
        abort = throwIO $ FatalError $ "While configuring the mount for effect sandbox path " <> show mntPath <> ", a mountable with name " <> mntName <> " has not been configured on agent, or it has been configured, but the condition field does not allow it to be used by this effect invocation. Make sure that mountable " <> mntName <> " exists in the agent configuration and that its condition field allows it to be used in the context of this job."
    case M.lookup mntName configuredMnts of
      Nothing -> do
        abort
      Just mountable -> do
        when (not ("/" `T.isPrefixOf` mntPath)) do
          throwIO $ FatalError ("Mount path must be absolute, but path does not start with /: " <> show mntPath)
        when ("/." `T.isInfixOf` mntPath) do
          throwIO $ FatalError ("Mount path must not contain /., but path is: " <> show mntPath)
        when ("//" `T.isInfixOf` mntPath) do
          throwIO $ FatalError ("Mount path must not contain //, but path is: " <> show mntPath)
        let -- Only valid after checks above
            checkPrefix path = do
              when (mntPath == path || (path <> "/") `T.isPrefixOf` mntPath) do
                throwIO $ FatalError ("Mount over " <> path <> " is not allowed: " <> show mntPath)
        checkPrefix "/nix"
        checkPrefix "/secrets"
        checkPrefix "/build"

        let cond = Mountable.condition mountable
        -- TODO: make hci effect run allow this? allow this when condition is simply `true`?
        ctx <- maybe (panic "No job context provided - don't know whether mounts are allowed.") pure secretContext
        let conditionOk = evalCondition ctx cond
        when (not conditionOk) do
          abort
        pure [BindMount {pathInContainer = mntPath, pathInHost = Mountable.source mountable, readOnly = Mountable.readOnly mountable}]

withNixDaemonProxy :: [(Text, Text)] -> FilePath -> IO a -> IO a
withNixDaemonProxy extraNixOptions socketPath wrappedAction = do
  -- Open the socket asap, so we don't have to wait for
  -- a readiness signal from the daemon, or poll, etc.
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix socketPath)
  listen sock 100

  -- (Ab)use stdin to transfer the socket while securely
  -- closing all other fds
  socketAsHandle <- withFdSocket sock \fd -> do
    fd' <- dup (fromIntegral fd)
    fdToHandle fd'

  exe <- WorkerProcess.getDaemonExe
  let opts = extraNixOptions >>= \(k, v) -> ["--option", toS k, toS v]
      procSpec =
        (Process.proc exe opts)
          { -- close all other fds to be secure
            Process.close_fds = True,
            Process.std_in = Process.UseHandle socketAsHandle,
            Process.std_err = Process.Inherit,
            Process.std_out = Process.UseHandle stderr
          }
  withCreateProcess procSpec $ \_in _out _err processHandle -> do
    wrappedAction
      -- TODO kill process _group_?
      `finally` destroyProcess_1s processHandle

forPid :: (Num pid) => ProcessHandle -> (pid -> IO ()) -> IO ()
forPid ph f = Process.Internal.withProcessHandle ph \case
  Process.Internal.OpenHandle {phdlProcessHandle = pid} -> f (fromIntegral pid)
  Process.Internal.OpenExtHandle {phdlProcessHandle = pid} -> f (fromIntegral pid)
  Process.Internal.ClosedHandle {} -> pure ()

-- | Make sure a process is terminated, in about 1s or less.
destroyProcess_1s :: ProcessHandle -> IO ()
destroyProcess_1s ph = do
  Process.terminateProcess ph
  let waitp = Process.waitForProcess ph
      killp = do
        threadDelay 500_000
        Process.terminateProcess ph
        threadDelay 500_000
        forPid ph \pid ->
          signalProcess killProcess pid
      handleKillException =
        handleJust
          ( \e -> case fromException e of
              -- completely ignore when cancelled by waitp completing early
              Just AsyncCancelled -> Nothing
              -- completely ignore asynchronous exceptions
              Nothing | isAsyncException e -> Nothing
              -- process may not exist anymore when sending a second signal (or more)
              -- in which case usually waitp will catch that during our threadDelay,
              -- but we shouldn't rely on that, as we don't want to raise false positives.
              Nothing | Just e' <- fromException e, isDoesNotExistError e' -> Nothing
              Nothing -> Just e
          )
          ( \e -> do
              -- TODO katip
              putErrLn ("hercules-ci-agent: Ignoring exception while stopping nix-daemon proxy " <> displayException e)
          )
  race_ waitp (killp & handleKillException)
