module Hercules.CLI.Login (commandParser) where

import qualified Options.Applicative as Optparse
import Protolude
import RIO (RIO)

commandParser :: Optparse.Parser (RIO r ())
commandParser = pure pass
