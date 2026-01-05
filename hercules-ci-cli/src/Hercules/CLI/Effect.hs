{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.CLI.Effect (commandParser) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Has (Has)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent as AttributeEffectEvent
import Hercules.API.Attribute (attributePathFromString)
import Hercules.API.Id (Id (Id, idUUID))
import qualified Hercules.API.Projects as Projects
import qualified Hercules.API.Projects.CreateUserEffectTokenResponse as CreateUserEffectTokenResponse
import Hercules.Agent.NixFile (getVirtualValueByPath)
import qualified Hercules.Agent.NixFile.GitSource as GitSource
import qualified Hercules.Agent.NixFile.HerculesCIArgs as HerculesCIArgs
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, determineDefaultApiBaseUrl, projectsClient, retryOnFail)
import Hercules.CLI.Common (runAuthenticatedOrDummy)
import Hercules.CLI.Exception (exitMsg)
import Hercules.CLI.Git (getAllBranches, getHypotheticalRefs)
import qualified Hercules.CLI.Git as Git
import Hercules.CLI.JSON (askPasswordWithKey)
import Hercules.CLI.Nix (ciNixAttributeCompleter, computeRef, createHerculesCIArgs, resolveInputs, withNix)
import Hercules.CLI.Options (flatCompleter, mkCommand, subparser)
import Hercules.CLI.Project (ProjectPath, getProjectIdAndPath, projectOption, projectPathOwner, projectPathProject, projectPathText)
import Hercules.CLI.Secret (getSecretsFilePath)
import Hercules.CNix (Store)
import Hercules.CNix.Expr (EvalState, Match (IsAttrs), Value (rtValue), getAttrBool, getDrvFile, match)
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store (Derivation, buildPaths, getDerivationInputs, newStorePathWithOutputs)
import qualified Hercules.CNix.Store as CNix
import Hercules.Effect (RunEffectParams (..), parseDrvSecretsMap, runEffect)
import Hercules.Error (escalate)
import qualified Hercules.Secrets as Secret
import Katip (initLogEnv, runKatipContextT)
import Options.Applicative (completer, help, long, metavar, strArgument, strOption)
import qualified Options.Applicative as Optparse
import Protolude hiding (evalState, wait, withAsync)
import RIO (RIO, askUnliftIO)
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import UnliftIO.Temporary (withTempDirectory)

commandParser :: Optparse.Parser (IO ())
commandParser =
  subparser
    ( mkCommand
        "run"
        (Optparse.progDesc "Run an effect")
        runParser
    )

-- Common options for both run and eval
data EffectOptions = EffectOptions
  { eoAttribute :: Text,
    eoProjectPath :: Maybe ProjectPath,
    eoRef :: Maybe Text,
    eoRequireToken :: Bool
  }

effectOptionsParser :: Optparse.Parser EffectOptions
effectOptionsParser = do
  attr <- ciAttributeArgument
  projectOptionMaybe <- optional projectOption
  refMaybe <- asRefOptions
  requireToken <- Optparse.flag True False (long "no-token" <> help "Don't get an API token. Disallows access to state files, but can run in untrusted environment or unconfigured repo.")
  pure $ EffectOptions attr projectOptionMaybe refMaybe requireToken

runParser :: Optparse.Parser (IO ())
runParser = do
  opts <- effectOptionsParser
  pure $ runAuthenticatedOrDummy (eoRequireToken opts) do
    -- Check if it's a direct derivation path
    isDirectPath <- withNix \store _evalState -> do
      storeDir <- liftIO $ CNix.storeDir store
      pure $ decodeUtf8With lenientDecode storeDir `T.isPrefixOf` (eoAttribute opts)

    if isDirectPath
      then do
        -- Direct derivation path - handle specially
        withNix \store _evalState -> do
          ref <- liftIO $ computeRef (eoRef opts)
          derivation <- liftIO $ do
            -- Support derivation in arbitrary location
            -- Used in hercules-ci-effects test runner
            let path = eoAttribute opts
            contents <- BS.readFile $ toS path
            let stripDrv s = fromMaybe s (T.stripSuffix ".drv" s)
            CNix.getDerivationFromString store (path & T.takeWhileEnd (/= '/') & stripDrv & encodeUtf8) contents
          prepareDerivation store derivation
          runEffectImpl store derivation ref opts
      else do
        -- Normal effect attribute - use withEffectDerivation
        withEffectDerivation opts $ \store _evalState drvPath ref -> do
          -- Get the full derivation from the path
          derivation <- liftIO $ CNix.getDerivation store drvPath
          prepareDerivation store derivation
          runEffectImpl store derivation ref opts

-- Execute an effect derivation
runEffectImpl :: Store -> Derivation -> Text -> EffectOptions -> RIO (HerculesClientToken, HerculesClientEnv) ()
runEffectImpl _store derivation ref opts = do
  let getProjectInfo =
        case eoProjectPath opts of
          Just x
            | not (eoRequireToken opts) ->
                pure
                  ProjectData
                    { pdProjectPath = Just x,
                      pdProjectId = Nothing,
                      pdToken = Nothing
                    }
          _ -> getProjectEffectData (eoProjectPath opts) (eoRequireToken opts)
  withAsync getProjectInfo \projectPathAsync -> do
    isDefaultBranch <-
      if eoRequireToken opts
        then liftIO Git.getIsDefault
        else pure True

    drvEnv <- liftIO $ CNix.getDerivationEnv derivation
    secretsMap <- case parseDrvSecretsMap drvEnv of
      Left e -> exitMsg e
      Right r -> pure r
    serverSecrets <- loadServerSecrets secretsMap

    apiBaseURL <- liftIO determineDefaultApiBaseUrl
    ProjectData {pdProjectPath = projectPath, pdProjectId = projectId, pdToken = token} <- wait projectPathAsync
    secretsJson <- liftIO $ traverse getSecretsFilePath projectPath

    logEnv <- liftIO $ initLogEnv mempty "hci"
    -- withSystemTempDirectory "hci":
    --     ERRO[0000] container_linux.go:370: starting container process caused: process_linux.go:459: container init caused: rootfs_linux.go:59: mounting "/run/user/1000/hci6017/secrets" to rootfs at "/run/user/1000/hci6017/runc-state/rootfs/secrets" caused: operation not permitted
    dataDir <- liftIO $ getAppUserDataDirectory "hercules-ci"
    createDirectoryIfMissing True dataDir
    let secretContextMaybe =
          projectPath <&> \p ->
            Secret.SecretContext
              { ownerName = projectPathOwner p,
                repoName = projectPathProject p,
                isDefaultBranch = isDefaultBranch,
                ref = ref
              }
    exitCode <- withTempDirectory dataDir "tmp-effect-" \workDir -> do
      runKatipContextT logEnv () mempty $
        runEffect
          RunEffectParams
            { runEffectDerivation = derivation,
              runEffectToken = token,
              runEffectSecretsConfigPath = secretsJson,
              runEffectServerSecrets = serverSecrets,
              runEffectApiBaseURL = apiBaseURL,
              runEffectDir = workDir,
              runEffectProjectId = projectId,
              runEffectProjectPath = projectPathText <$> projectPath,
              runEffectSecretContext = secretContextMaybe,
              runEffectUseNixDaemonProxy = False, -- FIXME Enable proxy for ci/dev parity. Requires access to agent binaries. Unified executable?
              runEffectExtraNixOptions = [],
              runEffectFriendly = True,
              runEffectConfiguredMountables = mempty -- FIXME: provide hosts?
            }
    throwIO exitCode

loadServerSecrets :: Map Text AttributeEffectEvent.SecretRef -> RIO r (Sensitive (Map Text (Map Text A.Value)))
loadServerSecrets secrets = secrets & M.traverseMaybeWithKey loadServerSecret <&> Sensitive

loadServerSecret :: Text -> AttributeEffectEvent.SecretRef -> RIO r (Maybe (Map Text A.Value))
loadServerSecret name sr = case sr of
  AttributeEffectEvent.SimpleSecret _ -> pure Nothing
  AttributeEffectEvent.GitToken gitToken -> loadGitToken name gitToken

loadGitToken :: Text -> AttributeEffectEvent.GitToken -> RIO r (Maybe (Map Text A.Value))
loadGitToken name _noDetail = do
  -- TODO: read gh hosts.yaml file?
  token <- liftIO $ askPasswordWithKey (Just name) "token"
  purer $
    M.fromList
      [token <&> A.String]

-- Higher-order function to evaluate effect and pass its path to a callback
withEffectDerivation :: EffectOptions -> (Store -> Ptr EvalState -> CNix.StorePath -> Text -> RIO (HerculesClientToken, HerculesClientEnv) a) -> RIO (HerculesClientToken, HerculesClientEnv) a
withEffectDerivation opts callback = do
  withNix \store evalState -> do
    ref <- liftIO $ computeRef (eoRef opts)
    storeDir <- liftIO $ CNix.storeDir store
    drvPath <-
      if decodeUtf8With lenientDecode storeDir `T.isPrefixOf` (eoAttribute opts)
        then do
          -- Support derivation in arbitrary location
          -- Used in hercules-ci-effects test runner
          let path = eoAttribute opts
          liftIO $ CNix.parseStorePath store (encodeUtf8 path)
        else evaluateEffectPath evalState (eoProjectPath opts) ref (eoAttribute opts)
    callback store evalState drvPath ref

-- Shared logic to evaluate an effect attribute and get its derivation path
evaluateEffectPath :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Ptr EvalState -> Maybe ProjectPath -> Text -> Text -> RIO r CNix.StorePath
evaluateEffectPath evalState projectOptionMaybe ref attr = do
  args <- liftIO $ createHerculesCIArgs (Just ref)
  let attrPath = attributePathFromString attr
      nixFile = GitSource.outPath $ HerculesCIArgs.primaryRepo args
  uio <- askUnliftIO
  valMaybe <- liftIO $ getVirtualValueByPath evalState (toS nixFile) args (resolveInputs uio evalState projectOptionMaybe) (map encodeUtf8 attrPath)
  attrValue <- case valMaybe of
    Nothing -> exitMsg $ "Could not find an attribute at path " <> show attrPath <> " in " <> nixFile
    Just v -> liftIO (match evalState v) >>= escalate
  effectAttrs <- case attrValue of
    IsAttrs attrs -> pure attrs
    _ -> exitMsg $ "Attribute is not an Effect at path " <> show attrPath <> " in " <> nixFile
  isEffect <- liftIO $ getAttrBool evalState effectAttrs "isEffect" >>= escalate
  when (isEffect /= Just True) do
    exitMsg $ "Attribute is not an Effect at path " <> show attrPath <> " in " <> nixFile
  getDrvFile evalState (rtValue effectAttrs)

prepareDerivation :: (MonadIO m) => Store -> Derivation -> m ()
prepareDerivation store derivation = do
  inputs <- liftIO $ getDerivationInputs store derivation
  storePathsWithOutputs <- liftIO Std.Vector.new
  liftIO $ for_ inputs \(input, outputs) -> do
    swo <- newStorePathWithOutputs input outputs
    Std.Vector.pushBackFP storePathsWithOutputs swo
  liftIO $ buildPaths store storePathsWithOutputs

ciAttributeArgument :: Optparse.Parser Text
ciAttributeArgument =
  strArgument $
    metavar "CI_NIX_ATTRIBUTE"
      <> help "Attribute to run"
      <> completer ciNixAttributeCompleter

asBranchOption :: Optparse.Parser Text
asBranchOption =
  strOption (long "pretend-branch" <> metavar "BRANCH" <> help "Pretend we're on another git branch" <> completer (flatCompleter getAllBranches))
    <|> strOption (long "as-branch" <> metavar "BRANCH" <> help "Alias for --pretend-branch")

asRefOption :: Optparse.Parser Text
asRefOption =
  strOption (long "pretend-ref" <> metavar "REF" <> help "Pretend we're on another git ref" <> completer (flatCompleter getHypotheticalRefs))
    <|> strOption (long "as-ref" <> metavar "REF" <> help "Alias for --pretend-ref")

asRefOptions :: Optparse.Parser (Maybe Text)
asRefOptions = optional (asRefOption <|> (("refs/heads/" <>) <$> asBranchOption))

data ProjectData = ProjectData
  { pdProjectPath :: Maybe ProjectPath,
    pdProjectId :: Maybe (Id "project"),
    pdToken :: Maybe (Sensitive Text)
  }

getProjectEffectData :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> Bool -> RIO r ProjectData
getProjectEffectData maybeProjectPathParam requireToken = do
  (projectIdMaybe, path) <- getProjectIdAndPath maybeProjectPathParam
  if requireToken
    then do
      projectId <- case projectIdMaybe of
        Just x -> pure x
        Nothing -> do
          exitMsg $ "Can not access " <> show path <> ". Make sure you have installed Hercules CI on the organization and repository and that you have access to it."
      response <- retryOnFail "create user effect token" (Projects.createUserEffectToken projectsClient projectId)
      let token = Sensitive (CreateUserEffectTokenResponse.token response)
      pure ProjectData {pdProjectPath = Just path, pdProjectId = Just $ Id $ idUUID projectId, pdToken = Just token}
    else
      pure
        ProjectData
          { pdProjectPath = Nothing,
            pdProjectId = Nothing,
            pdToken = Nothing
          }
