{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Secret where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import Hercules.CLI.Common (exitMsg, runAuthenticated)
import Hercules.CLI.JSON as JSON
import Hercules.CLI.Options (mkCommand, subparser)
import Hercules.CLI.Project (ProjectPath (projectPathOwner, projectPathSite), getProjectPath, projectOption)
import qualified Options.Applicative as Optparse
import Protolude
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)

commandParser, initLocal, add :: Optparse.Parser (IO ())
commandParser =
  subparser
    ( mkCommand
        "init-local"
        (Optparse.progDesc "Create a local secrets file in ~/.config/hercules-ci/secrets/<site>/<owner>")
        initLocal
        <> mkCommand
          "add"
          (Optparse.progDesc "Insert a secret into the local secrets file")
          add
    )
initLocal = do
  projectOptionMaybe <- optional projectOption
  pure $ runAuthenticated do
    projectPath <- getProjectPath projectOptionMaybe
    secretsFilePath <- liftIO $ getSecretsFilePath projectPath
    doesFileExist secretsFilePath >>= \case
      True -> do
        putErrText $ "hci: Secrets file already existed. Path: " <> toS secretsFilePath
      False -> do
        liftIO $ createDirectoryIfMissing True (takeDirectory secretsFilePath)
        liftIO $ writeFile secretsFilePath "{}"
        putErrText $ "hci: Secrets file created. Path: " <> toS secretsFilePath
add = do
  projectOptionMaybe <- optional projectOption
  mkJson <- JSON.options
  secretName <- Optparse.strArgument (Optparse.metavar "SECRET_NAME" <> Optparse.help "Organization/account-wide name for the secret")
  pure $ runAuthenticated do
    secretData <- liftIO mkJson
    projectPath <- getProjectPath projectOptionMaybe
    secretsFilePath <- liftIO $ getSecretsFilePath projectPath
    liftIO $
      doesFileExist secretsFilePath >>= \case
        False -> exitMsg $ "No secrets file found. If the account is correct, use `hci init-local`. (path: " <> toS secretsFilePath <> ")"
        True -> pass
    secrets <- liftIO $ readJsonFile secretsFilePath
    case M.lookup secretName secrets of
      Just _ -> do
        exitMsg $ "Secret " <> secretName <> " already exists in " <> toS secretsFilePath <> "."
      Nothing -> pass
    let secrets' = secrets & M.insert secretName (A.object ["kind" A..= A.String "Secret", "data" A..= secretData])
    liftIO $ writeJsonFile secretsFilePath secrets'
    putErrText $ "hci: successfully wrote " <> secretName <> " to " <> toS secretsFilePath
    putErrText "NOTE: Remember to synchronize this file with your agents!"

getSecretsFilePath :: ProjectPath -> IO FilePath
getSecretsFilePath projectPath = do
  dir <- getXdgDirectory XdgConfig "hercules-ci"
  let toPathElement = toS . T.map (\case '/' -> '_'; x -> x)
  pure $ dir </> "secrets" </> toPathElement (projectPathSite projectPath) </> toPathElement (projectPathOwner projectPath) </> "secrets.json"
