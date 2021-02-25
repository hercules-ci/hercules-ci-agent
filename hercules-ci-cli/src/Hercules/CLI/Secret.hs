{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Secret where

import qualified Data.Text as T
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Options (mkCommand)
import Hercules.CLI.Project (ProjectPath (projectPathOwner, projectPathSite), getProjectPath, projectOption)
import qualified Options.Applicative as Optparse
import Protolude
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)

commandParser, initLocal :: Optparse.Parser (IO ())
commandParser =
  Optparse.subparser
    ( mkCommand
        "init-local"
        (Optparse.progDesc "Create a local secrets file in ~/.config/hercules-ci/secrets/<site>/<owner>")
        initLocal
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

getSecretsFilePath :: ProjectPath -> IO FilePath
getSecretsFilePath projectPath = do
  dir <- getXdgDirectory XdgConfig "hercules-ci"
  let toPathElement = toS . T.map (\case '/' -> '_'; x -> x)
  pure $ dir </> "secrets" </> toPathElement (projectPathSite projectPath) </> toPathElement (projectPathOwner projectPath) </> "secrets.json"
