{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module Hercules.CLI.Options where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Options.Applicative hiding (helper)
import qualified Options.Applicative as Optparse
#if MIN_VERSION_hercules_ci_optparse_applicative(0, 18, 0)
import Options.Applicative.Extra as Optparse hiding (helper)
#endif
import Protolude

-- | Custom execParser that provides help text when input is incomplete.
execParser :: ParserInfo a -> IO a
execParser = Optparse.customExecParser (Optparse.prefs Optparse.showHelpOnEmpty)

-- | We omit --help from the help text and completion.
helper :: Parser (a -> a)
helper = helperWith (short 'h' <> long "help" <> internal)

subparser :: Mod CommandFields a -> Parser a
subparser = Optparse.subparser

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
