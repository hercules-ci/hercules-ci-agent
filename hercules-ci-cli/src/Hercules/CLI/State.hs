{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hercules.CLI.State (commandParser, getProjectAndClient) where

import Conduit (mapC, runConduitRes, sinkLazyBuilder, (.|))
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Has (Has)
import Hercules.API (ClientAuth, NoContent, enterApiE)
import Hercules.API.Name (Name (Name))
import Hercules.API.State
import Hercules.CLI.Client
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Options (mkCommand, subparser)
import Hercules.CLI.Project (ProjectPath (projectPathOwner, projectPathProject, projectPathSite), findProjectContextually, projectOption)
import Options.Applicative (auto, bashCompleter, completer, help, long, metavar, option, strOption)
import qualified Options.Applicative as Optparse
import Protolude
import RIO (RIO)
import qualified RIO.ByteString as BS
import Servant.API (HList (HCons), Headers (Headers), ResponseHeader (Header), fromSourceIO)
import Servant.Client (ClientError (ConnectionError))
import Servant.Client.Generic (AsClientT)
import Servant.Client.Internal.HttpClient.Streaming (ClientM)
import Servant.Conduit ()
import qualified Servant.Types.SourceT as Servant

commandParser, getCommandParser, putCommandParser :: Optparse.Parser (IO ())
commandParser =
  subparser
    ( mkCommand
        "get"
        (Optparse.progDesc "Download a state file")
        getCommandParser
        <> mkCommand
          "put"
          (Optparse.progDesc "Upload a state file")
          putCommandParser
    )
getCommandParser = do
  projectMaybe <- optional projectOption
  name <- nameOption
  file <- fileOption
  versionMaybe <- optional versionOption
  pure do
    runAuthenticated do
      projectStateClient <- getProjectAndClient projectMaybe
      -- TODO: version
      bytes <- retryStreamOnFail "state get" (\token -> getStateData projectStateClient name versionMaybe token) \case
        Left e -> throwIO e
        Right (Headers stream headers) -> do
          bl <- runConduitRes $ fromSourceIO stream .| mapC (BB.byteString . fromRawBytes) .| sinkLazyBuilder
          let lenH :: ResponseHeader "Content-Length" Integer
              lenH `HCons` _ = headers
              actual = fromIntegral $ BL.length bl
          case lenH of
            Header expected ->
              when (actual /= expected) do
                throwIO $ ConnectionError $ toException $ FatalError $ "Expected " <> show expected <> " bytes, but got " <> show actual
            _ -> pass
          pure (BL.toStrict bl)
      case file of
        "-" -> BS.putStr bytes
        _ -> BS.writeFile file bytes
putCommandParser = do
  projectMaybe <- optional projectOption
  name <- nameOption
  file <- fileOption
  pure do
    runAuthenticated do
      projectStateClient <- getProjectAndClient projectMaybe
      bytes <- case file of
        "-" -> BS.getContents
        _ -> BS.readFile file
      _ :: NoContent <- retryOnFail "state put" do
        putStateData projectStateClient name (Servant.source [RawBytes bytes])
      putErrText $ "hci: State file upload successful for " <> name

nameOption :: Optparse.Parser Text
nameOption = strOption $ long "name" <> metavar "NAME" <> help "Name of the state file"

fileOption :: Optparse.Parser FilePath
fileOption = strOption $ long "file" <> metavar "FILE" <> help "Local path of the state file or - for stdio" <> completer (bashCompleter "file")

versionOption :: Optparse.Parser Int
versionOption = option auto $ long "version" <> metavar "INT" <> help "Version of the state file to retrieve"

getProjectAndClient :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> RIO r (ProjectStateResourceGroup ClientAuth (AsClientT ClientM))
getProjectAndClient projectMaybe =
  case projectMaybe of
    Just projectPath ->
      pure (stateClient `enterApiE` \api -> byProjectName api (Name $ projectPathSite projectPath) (Name $ projectPathOwner projectPath) (Name $ projectPathProject projectPath))
    Nothing -> do
      (projectIdMaybe, projectPath) <- findProjectContextually
      case projectIdMaybe of
        Just projectId ->
          pure (stateClient `enterApiE` \api -> byProjectId api projectId)
        Nothing ->
          pure (stateClient `enterApiE` \api -> byProjectName api (Name $ projectPathSite projectPath) (Name $ projectPathOwner projectPath) (Name $ projectPathProject projectPath))
