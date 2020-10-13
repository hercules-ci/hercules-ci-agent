module Hercules.CLI.Main (main) where

import qualified Hercules.CLI.Login as Login
import qualified Hercules.CLI.State as State
import qualified Options.Applicative as Optparse
import Protolude
import RIO (RIO, runRIO)

main :: IO ()
main = do
  app <- init
  runRIO app =<< Optparse.execParser opts

data MainEnv = MainEnv

init :: IO MainEnv
init = pure MainEnv

opts :: Optparse.ParserInfo (RIO MainEnv ())
opts =
  Optparse.info
    (commands <**> Optparse.helper)
    (Optparse.fullDesc <> Optparse.header "Command line interface to Hercules CI")

commands :: Optparse.Parser (RIO MainEnv ())
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

mkCommand ::
  [Char] ->
  Optparse.InfoMod a ->
  Optparse.Parser a ->
  Optparse.Mod Optparse.CommandFields a
mkCommand name infos cmdParser =
  Optparse.command
    name
    ( Optparse.info
        (Optparse.helper <*> cmdParser)
        infos
    )
