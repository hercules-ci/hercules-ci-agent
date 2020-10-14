module Hercules.CLI.Main (main) where

import qualified Hercules.CLI.Login as Login
import qualified Hercules.CLI.State as State
import qualified Options.Applicative as Optparse
import Protolude

main :: IO ()
main = do
  join $ Optparse.execParser opts

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
