{-# LANGUAGE BlockArguments #-}

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

scanArguments :: Text -> [Text] -> [Text]
scanArguments opt (opt' : val : opts) | opt == opt' = val : scanArguments opt opts
scanArguments opt (_ : opts) = scanArguments opt opts
scanArguments _ _ = []

getCompletionWords :: IO [Text]
getCompletionWords = scanArguments "--bash-completion-word" . fmap toS <$> getArgs

scanOption :: Text -> IO (Maybe Text)
scanOption opt = do
  lastMay . scanArguments opt <$> getCompletionWords

flatCompleter :: IO [Text] -> Completer
flatCompleter generate = mkCompleter \prefix -> do
  items <- generate
  pure $
    items
      & filter (T.isPrefixOf (toS prefix))
      & map toS
