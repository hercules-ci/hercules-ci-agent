module Hercules.CLI.Options where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Options.Applicative
import Protolude

mkCommand ::
  [Char] ->
  InfoMod a ->
  Parser a ->
  Mod CommandFields a
mkCommand name infos cmdParser =
  command name (info (helper <*> cmdParser) infos)

packSome :: A.Parser Char -> A.Parser Text
packSome = fmap T.pack . some

attoparsecReader :: A.Parser a -> ReadM a
attoparsecReader p = eitherReader (A.parseOnly p . T.pack)
