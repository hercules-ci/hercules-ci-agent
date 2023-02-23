{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.CLI.Effect where

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
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, determineDefaultApiBaseUrl, projectsClient, runHerculesClient)
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

commandParser, runParser :: Optparse.Parser (IO ())
commandParser =
  subparser
    ( mkCommand
        "run"
        (Optparse.progDesc "Run an effect")
        runParser
    )
runParser = do
  attr <- ciAttributeArgument
  projectOptionMaybe <- optional projectOption
  refMaybe <- asRefOptions
  requireToken <- Optparse.flag True False (long "no-token" <> help "Don't get an API token. Disallows access to state files, but can run in untrusted environment or unconfigured repo.")
  pure $ runAuthenticatedOrDummy requireToken do
    let getProjectInfo =
          case projectOptionMaybe of
            Just x
              | not requireToken ->
                  pure
                    ProjectData
                      { pdProjectPath = Just x,
                        pdProjectId = Nothing,
                        pdToken = Nothing
                      }
            _ -> getProjectEffectData projectOptionMaybe requireToken
    withAsync getProjectInfo \projectPathAsync -> do
      withNix \store evalState -> do
        ref <- liftIO $ computeRef refMaybe
        derivation <- getEffectDrv store evalState projectOptionMaybe ref attr
        isDefaultBranch <-
          if requireToken
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
                  runEffectFriendly = True
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

getEffectDrv :: Store -> Ptr EvalState -> Maybe ProjectPath -> Text -> Text -> RIO (HerculesClientToken, HerculesClientEnv) Derivation
getEffectDrv store evalState projectOptionMaybe ref attr = do
  storeDir <- liftIO $ CNix.storeDir store
  derivation <-
    if decodeUtf8With lenientDecode storeDir `T.isPrefixOf` attr
      then liftIO $ do
        -- Support derivation in arbitrary location
        -- Used in hercules-ci-effects test runner
        let path = attr
        contents <- BS.readFile $ toS path
        let stripDrv s = fromMaybe s (T.stripSuffix ".drv" s)
        CNix.getDerivationFromString store (path & T.takeWhileEnd (/= '/') & stripDrv & encodeUtf8) contents
      else evaluateEffectDerivation evalState store projectOptionMaybe ref attr
  prepareDerivation store derivation
  pure derivation

evaluateEffectDerivation :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Ptr EvalState -> Store -> Maybe ProjectPath -> Text -> Text -> RIO r Derivation
evaluateEffectDerivation evalState store projectOptionMaybe ref attr = do
  args <- liftIO $ createHerculesCIArgs (Just ref)
  let attrPath = attributePathFromString attr
      nixFile = GitSource.outPath $ HerculesCIArgs.primaryRepo args
  uio <- askUnliftIO
  valMaybe <- liftIO $ getVirtualValueByPath evalState (toS nixFile) args (resolveInputs uio evalState projectOptionMaybe) (map encodeUtf8 attrPath)
  -- valMaybe <- liftIO $ attrByPath evalState rootValue
  attrValue <- case valMaybe of
    Nothing -> do
      exitMsg $ "Could not find an attribute at path " <> show attrPath <> " in " <> nixFile
    Just v -> liftIO (match evalState v) >>= escalate
  effectAttrs <- case attrValue of
    IsAttrs attrs -> pure attrs
    _ -> do
      exitMsg $ "Attribute is not an Effect at path " <> show attrPath <> " in " <> nixFile

  isEffect <- liftIO $ getAttrBool evalState effectAttrs "isEffect" >>= escalate
  when (isEffect /= Just True) do
    exitMsg $ "Attribute is not an Effect at path " <> show attrPath <> " in " <> nixFile
  drvPath <- getDrvFile evalState (rtValue effectAttrs)
  liftIO $ CNix.getDerivation store drvPath

prepareDerivation :: MonadIO m => Store -> Derivation -> m ()
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
asBranchOption = strOption $ long "as-branch" <> metavar "BRANCH" <> help "Pretend we're on another git branch" <> completer (flatCompleter getAllBranches)

asRefOption :: Optparse.Parser Text
asRefOption = strOption $ long "as-ref" <> metavar "REF" <> help "Pretend we're on another git ref" <> completer (flatCompleter getHypotheticalRefs)

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
      response <- runHerculesClient (Projects.createUserEffectToken projectsClient projectId)
      let token = Sensitive (CreateUserEffectTokenResponse.token response)
      pure ProjectData {pdProjectPath = Just path, pdProjectId = Just $ Id $ idUUID projectId, pdToken = Just token}
    else
      pure
        ProjectData
          { pdProjectPath = Nothing,
            pdProjectId = Nothing,
            pdToken = Nothing
          }
