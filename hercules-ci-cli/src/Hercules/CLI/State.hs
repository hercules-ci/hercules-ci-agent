{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.State where

import Conduit (ConduitT, mapC, runConduitRes, sinkFile, sourceHandle, stdinC, stdoutC, (.|))
import qualified Hercules.API.Projects.Project as Project
import Hercules.API.State
import Hercules.CLI.Client
import Hercules.CLI.Credentials
import Hercules.CLI.Options (mkCommand)
import Hercules.CLI.Project (findProject, projectOption)
import Options.Applicative (bashCompleter, completer, help, long, metavar, strOption)
import qualified Options.Applicative as Optparse
import Protolude hiding (option)
import RIO (RIO, runRIO, withBinaryFile)
import Servant.API (Headers (Headers), fromSourceIO, toSourceIO)
import Servant.Auth.Client (Token (Token))
import Servant.Conduit ()

commandParser, getCommandParser, putCommandParser :: Optparse.Parser (IO ())
commandParser =
  Optparse.subparser
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
  project <- projectOption
  name <- nameOption
  file <- fileOption
  pure do
    clientEnv <- Hercules.CLI.Client.init
    domain <- determineDomain
    token <- readPersonalToken domain
    runRIO (HerculesClientToken $ Token $ encodeUtf8 $ token, clientEnv) do
      projectId <- Project.id <$> findProject project
      runHerculesClientStream (getProjectStateData stateClient projectId name) \case
        Left e -> dieWithHttpError e
        Right (Headers r _) -> do
          runConduitRes $
            fromSourceIO r .| mapC fromRawBytes .| case file of
              "-" -> stdoutC
              _ -> sinkFile file
putCommandParser = do
  project <- projectOption
  name <- nameOption
  file <- fileOption
  pure do
    clientEnv <- Hercules.CLI.Client.init
    domain <- determineDomain
    token <- readPersonalToken domain
    runRIO (HerculesClientToken $ Token $ encodeUtf8 $ token, clientEnv) do
      projectId <- Project.id <$> findProject project
      let withStream :: (ConduitT a RawBytes IO () -> RIO r b) -> RIO r b
          withStream = case file of
            "-" -> ($ (stdinC .| mapC RawBytes))
            _ -> \f -> do
              r <- ask
              liftIO $ withBinaryFile file ReadMode \h ->
                runRIO r $ f (sourceHandle h .| mapC RawBytes)
      withStream \stream -> do
        _noContent <- runHerculesClient (putProjectStateData stateClient projectId name (toSourceIO (stream)))
        pass

nameOption :: Optparse.Parser Text
nameOption = strOption $ long "name" <> metavar "NAME" <> help "Name of the state file"

fileOption :: Optparse.Parser FilePath
fileOption = strOption $ long "file" <> metavar "FILE" <> help "Local path of the state file or - for stdio" <> completer (bashCompleter "file")
