{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Effect where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent (GitToken (..), SecretRef (GitToken, SimpleSecret), SimpleSecret (MkSimpleSecret))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent
import Hercules.API.Id (Id, idText)
import Hercules.Agent.Sensitive (Sensitive (Sensitive, reveal), revealContainer)
import qualified Hercules.Agent.WorkerProcess as WorkerProcess
import Hercules.CNix (Derivation)
import Hercules.CNix.Store (getDerivationArguments, getDerivationBuilder, getDerivationEnv)
import Hercules.Effect.Container (BindMount (BindMount))
import qualified Hercules.Effect.Container as Container
import Hercules.Error (escalateAs)
import qualified Hercules.Formats.Secret as Formats.Secret
import Hercules.Secrets (SecretContext, evalCondition, evalConditionTrace)
import Katip (KatipContext, Severity (..), logLocM, logStr)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), bind, listen, socket, withFdSocket)
import Protolude
import System.FilePath
import System.Posix (dup, fdToHandle)
import UnliftIO.Directory (createDirectory, createDirectoryIfMissing)
import UnliftIO.Process (withCreateProcess)
import qualified UnliftIO.Process as Process

parseDrvSecretsMap :: Map ByteString ByteString -> Either Text (Map Text SecretRef)
parseDrvSecretsMap drvEnv =
  case (,) "secretsToUse" <$> M.lookup "secretsToUse" drvEnv
    <|> (,) "secretsMap" <$> M.lookup "secretsMap" drvEnv of
    Nothing -> pure mempty
    Just (attrName, secretsMapText) -> case A.eitherDecode (BL.fromStrict secretsMapText) of
      Left _ -> Left $ "Could not parse " <> attrName <> " variable in derivation. It must be a JSON dictionary."
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
  let mkDir d = let newDir = dir </> d in toS newDir <$ createDirectory newDir
  buildDir <- mkDir "build"
  etcDir <- mkDir "etc"
  secretsDir <- mkDir "secrets"
  runcDir <- mkDir "runc-state"
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
        (//) :: Ord k => Map k a -> Map k a -> Map k a
        (//) = flip M.union
    let (withNixDaemonProxyPerhaps, forwardedSocketPath) =
          if runEffectUseNixDaemonProxy p
            then
              let socketPath = dir </> "nix-daemon-socket"
               in (withNixDaemonProxy (runEffectExtraNixOptions p) socketPath, socketPath)
            else (identity, "/nix/var/nix/daemon-socket/socket")

    withNixDaemonProxyPerhaps $
      Container.run
        runcDir
        Container.Config
          { extraBindMounts =
              [ BindMount {pathInContainer = "/build", pathInHost = buildDir, readOnly = False},
                BindMount {pathInContainer = "/etc", pathInHost = etcDir, readOnly = False},
                BindMount {pathInContainer = "/secrets", pathInHost = secretsDir, readOnly = True},
                -- we cannot bind mount this read-only because of https://github.com/opencontainers/runc/issues/1523
                BindMount {pathInContainer = "/etc/resolv.conf", pathInHost = "/etc/resolv.conf", readOnly = False},
                BindMount {pathInContainer = "/nix/var/nix/daemon-socket/socket", pathInHost = toS forwardedSocketPath, readOnly = True}
              ],
            executable = decodeUtf8With lenientDecode drvBuilder,
            arguments = map (decodeUtf8With lenientDecode) drvArgs,
            environment = overridableEnv // drvEnv' // onlyImpureOverridableEnv // impureEnvVars // fixedEnv,
            workingDirectory = "/build",
            hostname = "hercules-ci",
            rootReadOnly = False
          }

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
  withCreateProcess procSpec $ \_in _out _err _processHandle -> do
    wrappedAction
