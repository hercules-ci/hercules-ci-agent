{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Main
  ( main,
  )
where

import Hercules.CLI.Client (prettyPrintHttpErrors)
import qualified Hercules.CLI.Login as Login
import Hercules.CLI.Options (mkCommand)
import qualified Hercules.CLI.State as State
import qualified Options.Applicative as Optparse
import Protolude

main :: IO ()
main = prettyPrintErrors $ prettyPrintHttpErrors $ do
  join $ Optparse.execParser opts

prettyPrintErrors :: IO a -> IO a
prettyPrintErrors = handle \e ->
  case fromException e :: Maybe ExitCode of
    Just _ -> throwIO e
    Nothing -> do
      putErrLn $ "hci: " <> displayException e
      exitFailure

opts :: Optparse.ParserInfo (IO ())
opts =
  Optparse.info
    (commands <**> Optparse.helper)
    (Optparse.fullDesc <> Optparse.header "Command line interface to Hercules CI")

commands :: Optparse.Parser (IO ())
commands =
  Optparse.subparser
    ( mkCommand
        "login"
        (Optparse.progDesc "Configure token for authentication to hercules-ci.com")
        Login.commandParser
        <> mkCommand
          "state"
          (Optparse.progDesc "Perform operations on state files")
          State.commandParser
    )
