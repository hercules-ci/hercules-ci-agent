{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Effect where

import Data.Has (Has)
import qualified Data.Text as T
import qualified Hercules.API.Projects as Projects
import qualified Hercules.API.Projects.CreateUserEffectTokenResponse as CreateUserEffectTokenResponse
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, determineDefaultApiBaseUrl, projectsClient, runHerculesClient)
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Exception (exitMsg)
import Hercules.CLI.Git (getAllBranches, getHypotheticalRefs)
import Hercules.CLI.Nix (attrByPath, callCiNix, ciNixAttributeCompleter, withNix)
import Hercules.CLI.Options (flatCompleter, mkCommand)
import Hercules.CLI.Project (ProjectPath, getProjectIdAndPath, projectOption)
import Hercules.CLI.Secret (getSecretsFilePath)
import Hercules.CNix (Store)
import Hercules.CNix.Expr (Match (IsAttrs), Value (rtValue), getAttrBool, getDrvFile, match)
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store (Derivation, StorePath, buildPaths, getDerivationInputs, newStorePathWithOutputs)
import qualified Hercules.CNix.Store as CNix
import Hercules.Effect (runEffect)
import Hercules.Error (escalate)
import Katip (initLogEnv, runKatipContextT)
import Options.Applicative (completer, help, long, metavar, strArgument, strOption)
import qualified Options.Applicative as Optparse
import Protolude hiding (evalState, wait, withAsync)
import RIO (RIO)
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import UnliftIO.Temporary (withTempDirectory)

commandParser, runParser :: Optparse.Parser (IO ())
commandParser =
  Optparse.subparser
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
  pure $ runAuthenticated do
    withAsync (getProjectEffectData projectOptionMaybe requireToken) \projectPathAsync -> do
      withNix \store evalState -> do
        (nixFile, rootValue) <- liftIO $ callCiNix evalState refMaybe
        let attrPath = T.split (== '.') attr
        valMaybe <- liftIO $ attrByPath evalState rootValue (map encodeUtf8 attrPath)
        attrValue <- case valMaybe of
          Nothing -> do
            exitMsg $ "Could not find an attribute at path " <> show attrPath <> " in " <> toS nixFile
          Just v -> liftIO (match evalState v) >>= escalate
        effectAttrs <- case attrValue of
          IsAttrs attrs -> pure attrs
          _ -> do
            exitMsg $ "Attribute is not an Effect at path " <> show attrPath <> " in " <> toS nixFile

        isEffect <- liftIO $ getAttrBool evalState effectAttrs "isEffect" >>= escalate
        when (isEffect /= Just True) do
          exitMsg $ "Attribute is not an Effect at path " <> show attrPath <> " in " <> toS nixFile
        drvPath <- getDrvFile evalState (rtValue effectAttrs)
        derivation <- prepareDerivation store drvPath
        apiBaseURL <- liftIO determineDefaultApiBaseUrl
        (projectPath, token) <- wait projectPathAsync
        secretsJson <- liftIO $ getSecretsFilePath projectPath
        logEnv <- liftIO $ initLogEnv mempty "hci"
        -- withSystemTempDirectory "hci":
        --     ERRO[0000] container_linux.go:370: starting container process caused: process_linux.go:459: container init caused: rootfs_linux.go:59: mounting "/run/user/1000/hci6017/secrets" to rootfs at "/run/user/1000/hci6017/runc-state/rootfs/secrets" caused: operation not permitted
        dataDir <- liftIO $ getAppUserDataDirectory "hercules-ci"
        createDirectoryIfMissing True dataDir
        exitCode <- withTempDirectory dataDir "tmp-effect-" \workDir -> do
          runKatipContextT logEnv () mempty $ runEffect derivation (Sensitive token) secretsJson apiBaseURL workDir
        throwIO exitCode

prepareDerivation :: MonadIO m => Store -> StorePath -> m Derivation
prepareDerivation store drvPath = do
  derivation <- liftIO $ CNix.getDerivation store drvPath
  inputs <- liftIO $ getDerivationInputs derivation
  storePathsWithOutputs <- liftIO Std.Vector.new
  liftIO $ for_ inputs \(input, outputs) -> do
    swo <- newStorePathWithOutputs input outputs
    Std.Vector.pushBackFP storePathsWithOutputs swo
  liftIO $ buildPaths store storePathsWithOutputs
  pure derivation

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

getProjectEffectData :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> Bool -> RIO r (ProjectPath, Text)
getProjectEffectData maybeProjectPathParam requireToken = do
  (projectIdMaybe, path) <- getProjectIdAndPath maybeProjectPathParam
  if requireToken
    then do
      projectId <- case projectIdMaybe of
        Just x -> pure x
        Nothing -> do
          exitMsg $ "Can not access " <> show path <> ". Make sure you have installed Hercules CI on the organization and repository and that you have access to it."
      response <- runHerculesClient (Projects.createUserEffectToken projectsClient projectId)
      let token = CreateUserEffectTokenResponse.token response
      pure (path, token)
    else pure (path, "")
