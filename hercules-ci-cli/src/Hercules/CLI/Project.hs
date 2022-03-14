{-# LANGUAGE TypeFamilies #-}

module Hercules.CLI.Project where

import qualified Data.Attoparsec.Text as A
import Data.Has (Has)
import qualified Data.UUID
import Hercules.API (ClientAuth, Id, enterApiE)
import Hercules.API.Id (Id (Id))
import Hercules.API.Name (Name (Name))
import Hercules.API.Projects (ProjectResourceGroup, ProjectsAPI (byProjectName), findProjects)
import qualified Hercules.API.Projects as Projects
import Hercules.API.Projects.Project (Project)
import qualified Hercules.API.Projects.Project as Project
import qualified Hercules.API.Repos as Repos
import qualified Hercules.API.Repos.RepoKey as RepoKey
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, projectsClient, reposClient, runHerculesClient, runHerculesClientEither)
import Hercules.CLI.Common (exitMsg)
import qualified Hercules.CLI.Git as Git
import Hercules.CLI.Options (attoparsecReader, packSome)
import Hercules.Error (escalate, escalateAs)
import Network.HTTP.Types (Status (Status, statusCode))
import Options.Applicative (bashCompleter, completer, help, long, metavar, option, strOption)
import qualified Options.Applicative as Optparse
import Protolude hiding (option)
import RIO (RIO)
import Servant.Client.Core (ClientError (FailureResponse), ResponseF (responseStatusCode))
import Servant.Client.Core.Response (ResponseF (Response))
import Servant.Client.Generic (AsClientT)
import Servant.Client.Streaming (ClientM)
import UnliftIO.Environment (lookupEnv)
import qualified Prelude

data ProjectPath = ProjectPath
  { projectPathSite :: Text,
    projectPathOwner :: Text,
    projectPathProject :: Text
  }

instance Prelude.Show ProjectPath where
  show = toS . projectPathText

projectPathText :: ProjectPath -> Text
projectPathText = projectPathSite <> const "/" <> projectPathOwner <> const "/" <> projectPathProject

projectOption :: Optparse.Parser ProjectPath
projectOption =
  option projectPathReadM $
    long "project" <> metavar "PROJECT" <> help "Project path, e.g. github/my-org/my-project"

nameOption :: Optparse.Parser Text
nameOption = strOption $ long "name" <> metavar "NAME" <> help "Name of the state file"

fileOption :: Optparse.Parser FilePath
fileOption = strOption $ long "file" <> metavar "FILE" <> help "Local path of the state file or - for stdio" <> completer (bashCompleter "file")

projectPathReadM :: Optparse.ReadM ProjectPath
projectPathReadM = attoparsecReader parseProjectPath

parseProjectPath :: A.Parser ProjectPath
parseProjectPath =
  pure ProjectPath
    <*> packSome (A.satisfy (/= '/'))
    <* A.char '/'
    <*> packSome (A.satisfy (/= '/'))
    <* A.char '/'
    <*> packSome (A.satisfy (/= '/'))

parseProjectPathFromText :: Text -> Either [Char] ProjectPath
parseProjectPathFromText = A.parseOnly parseProjectPath

getProjectPath :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> RIO r ProjectPath
getProjectPath maybeProjectPathParam =
  case maybeProjectPathParam of
    Nothing -> snd <$> findProjectContextually
    Just projectKey -> pure projectKey

getProjectIdAndPath :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> RIO r (Maybe (Id Project), ProjectPath)
getProjectIdAndPath maybeProjectPathParam = do
  case maybeProjectPathParam of
    Nothing -> findProjectContextually
    Just projectKey -> do
      project <- findProjectByKey projectKey
      pure (Project.id <$> project, projectKey)

findProjectByKey :: (Has HerculesClientToken r, Has HerculesClientEnv r) => ProjectPath -> RIO r (Maybe Project.Project)
findProjectByKey path =
  runHerculesClient
    ( Projects.findProjects
        projectsClient
        (Just $ Name $ projectPathSite path)
        (Just $ Name $ projectPathOwner path)
        (Just $ Name $ projectPathProject path)
    )
    <&> head

findProjectContextually :: (Has HerculesClientToken r, Has HerculesClientEnv r) => RIO r (Maybe (Id Project), ProjectPath)
findProjectContextually = do
  projectIdMaybe <- lookupEnv "HERCULES_CI_PROJECT_ID"
  projectIdPathMaybe <- lookupEnv "HERCULES_CI_PROJECT_PATH"
  case (,) <$> projectIdMaybe <*> projectIdPathMaybe of
    Nothing -> findProjectByCurrentRepo
    Just (id, pathText) -> do
      projectPath <- parseProjectPathFromText (toS pathText) & escalateAs (\e -> FatalError $ "Invalid HERCULES_CI_PROJECT_PATH supplied: " <> toS e)
      uuid <- Data.UUID.fromString id & maybeToEither (FatalError "Invalid UUID in HERCULES_CI_PROJECT_ID") & escalate
      pure (Just (Id uuid), projectPath)

findProjectByCurrentRepo :: (Has HerculesClientToken r, Has HerculesClientEnv r) => RIO r (Maybe (Id Project), ProjectPath)
findProjectByCurrentRepo = do
  url <- liftIO Git.getUpstreamURL
  rs <- runHerculesClientEither (Repos.parseGitURL reposClient url)
  case rs of
    Left (FailureResponse _req Response {responseStatusCode = Status {statusCode = 404}}) -> do
      exitMsg "Repository not recognized by Hercules CI. Make sure you're in the right repository, and if you're running Hercules CI Enterprise, make sure you're using the right HERCULES_CI_API_BASE_URL. Alternatively, use the --project option."
    Left e -> throwIO e
    Right r ->
      pure
        ( RepoKey.projectId r,
          ProjectPath
            { projectPathSite = RepoKey.siteName r,
              projectPathOwner = RepoKey.ownerName r,
              projectPathProject = RepoKey.repoName r
            }
        )

findProject :: (Has HerculesClientToken r, Has HerculesClientEnv r) => ProjectPath -> RIO r Project.Project
findProject project = do
  rs <-
    runHerculesClient
      ( findProjects
          projectsClient
          (Just $ Name $ projectPathSite project)
          (Just $ Name $ projectPathOwner project)
          (Just $ Name $ projectPathProject project)
      )
  case rs of
    [] -> do
      exitMsg $ "Project not found: " <> show project
    [p] -> pure p
    _ -> do
      exitMsg $ "Project ambiguous: " <> show project

projectResourceClientByPath :: ProjectPath -> ProjectResourceGroup ClientAuth (AsClientT ClientM)
projectResourceClientByPath projectPath =
  projectsClient `enterApiE` \api ->
    byProjectName
      api
      (Name $ projectPathSite projectPath)
      (Name $ projectPathOwner projectPath)
      (Name $ projectPathProject projectPath)
