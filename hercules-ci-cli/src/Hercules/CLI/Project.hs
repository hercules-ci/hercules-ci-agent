module Hercules.CLI.Project where

import qualified Data.Attoparsec.Text as A
import Data.Has (Has)
import Hercules.API.Name (Name (Name))
import Hercules.API.Projects (findProjects)
import qualified Hercules.API.Projects.Project as Project
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, projectsClient, runHerculesClient)
import Hercules.CLI.Options (attoparsecReader, packSome)
import qualified Options.Applicative as Optparse
import Options.Applicative (bashCompleter, completer, help, long, metavar, option, strOption)
import Protolude hiding (option)
import RIO (RIO)
import qualified Prelude

data ProjectPath
  = ProjectPath
      { projectPathSite :: Text,
        projectPathOwner :: Text,
        projectPathProject :: Text
      }

instance Prelude.Show ProjectPath where
  show = s projectPathSite <> const "/" <> s projectPathOwner <> const "/" <> s projectPathProject
    where
      s = (toS .)

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

findProject :: (Has HerculesClientToken r, Has HerculesClientEnv r) => ProjectPath -> RIO r (Project.Project)
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
      putErrText $ "Project not found: " <> show project
      liftIO exitFailure
    [p] -> pure p
    _ -> do
      putErrText $ "Project ambiguous: " <> show project
      liftIO exitFailure
