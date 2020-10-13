module Hercules.CLI.State where

import qualified Options.Applicative as Optparse
import Protolude
import RIO (RIO)

commandParser :: Optparse.Parser (RIO r ())
commandParser = panic "state command not implemented yet"
