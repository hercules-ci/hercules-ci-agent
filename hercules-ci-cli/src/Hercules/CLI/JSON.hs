{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.CLI.JSON where

import Data.Aeson
import Data.Aeson.Encode.Pretty (Indent (Spaces), confIndent, confTrailingNewline, defConfig, encodePretty')
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Hercules.UserException (UserException (UserException))
import qualified Options.Applicative as Optparse
import Protolude
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)
import System.Environment (getEnvironment, lookupEnv)

mergePaths :: [([Text], Value)] -> Either Text Value
mergePaths = mergeLeafPaths [] . toLeafPaths

mergeLeafPaths :: [Text] -> [([Text], Value)] -> Either Text Value
mergeLeafPaths _ [([], v)] = Right v
mergeLeafPaths context items =
  case for items (\(k, v) -> (,v) <$> NEL.nonEmpty k) of
    Nothing -> Left $ "Conflicting values for " <> showPath context
    Just nonRootItems ->
      nonRootItems
        & NEL.groupAllWith (NEL.head . fst)
        & traverse
          ( \(groupItem@(child :| _, _) :| groupItems) ->
              (child .=)
                <$> mergeLeafPaths (context ++ [child]) (map (first NEL.tail) (groupItem : groupItems))
          )
        <&> object

showPath :: [Text] -> Text
showPath [] = "the root"
showPath x = T.intercalate "." x

toLeafPaths :: [([Text], Value)] -> [([Text], Value)]
toLeafPaths = concatMap \(path, item) ->
  case item of
    Object fields ->
      fields & HM.toList & concatMap \(subpath, subitem) ->
        toLeafPaths [(path ++ [subpath], subitem)]
    _ -> [(path, item)]

options :: Optparse.Parser (IO Value)
options = do
  strings <-
    many
      ( Optparse.biOption
          Optparse.str
          Optparse.str
          ( Optparse.long "string"
              <> Optparse.help "Define a string at dot-separated PATH in the secret data"
              <> Optparse.metavar "PATH"
              <> Optparse.metavar2 "STRING"
          )
      )
  jsons <-
    many
      ( Optparse.biOption
          Optparse.str
          Optparse.str
          ( Optparse.long "json"
              <> Optparse.help "Define a JSON value at dot-separated PATH in the secret data"
              <> Optparse.metavar "PATH"
              <> Optparse.metavar2 "JSON"
          )
      )
  stringFiles <-
    many
      ( Optparse.biOption
          Optparse.str
          Optparse.str
          ( Optparse.long "string-file"
              <> Optparse.help "Define a string at dot-separated PATH in the secret data, by reading FILE"
              <> Optparse.metavar "PATH"
              <> Optparse.metavar2 "FILE"
              <> Optparse.completer2 (Optparse.bashCompleter "file")
          )
      )
  jsonFiles <-
    many
      ( Optparse.biOption
          Optparse.str
          Optparse.str
          ( Optparse.long "json-file"
              <> Optparse.help "Define a JSON value at dot-separated PATH in the secret data, by reading FILE"
              <> Optparse.metavar "PATH"
              <> Optparse.metavar2 "FILE"
              <> Optparse.completer2 (Optparse.bashCompleter "file")
          )
      )
  stringEnvs <-
    many
      ( Optparse.biOption
          Optparse.str
          Optparse.str
          ( Optparse.long "string-env"
              <> Optparse.help "Define a string at dot-separated PATH in the secret data, by reading environment variable ENV_NAME"
              <> Optparse.metavar "PATH"
              <> Optparse.metavar2 "ENV_NAME"
              <> Optparse.completer2 envCompleter
          )
      )
  jsonEnvs <-
    many
      ( Optparse.biOption
          Optparse.str
          Optparse.str
          ( Optparse.long "json-env"
              <> Optparse.help "Define a JSON value at dot-separated PATH in the secret data, by reading environment variable ENV_NAME"
              <> Optparse.metavar "PATH"
              <> Optparse.metavar2 "ENV_NAME"
              <> Optparse.completer2 envCompleter
          )
      )
  pure do
    fileStrings <- for stringFiles readFileWithKey
    fileJsons <- for jsonFiles readJsonFileWithKey
    envStrings <- for stringEnvs readEnvWithKey
    envJsons <- for jsonEnvs readJsonEnvWithKey
    validJsons <-
      for
        jsons
        ( \(key, value) ->
            case eitherDecode $ BL.fromStrict $ encodeUtf8 value of
              Left e -> throwIO $ UserException $ "Value for key " <> key <> " is not valid JSON: " <> show e
              Right r -> pure (key, r :: Value)
        )
    let items =
          map (first split) $
            (fmap String <$> strings)
              <> (fmap String <$> envStrings)
              <> (fmap String <$> fileStrings)
              <> validJsons
              <> envJsons
              <> fileJsons

        split "." = []
        split "" = []
        split p = T.split (== '.') p
    when (items & any \(path, _) -> path & any (T.any (== '\"'))) do
      throwIO $ FatalError "Quotes in field names are not allowed, so proper quotation can be implemented in the future. Write the field name in the value of --json or --json-file instead."
    case mergePaths items of
      Left e -> throwIO $ UserException $ show e
      Right r -> pure r

readJsonEnvWithKey :: (Text, [Char]) -> IO (Text, Value)
readJsonEnvWithKey (key, envVar) =
  lookupEnv envVar >>= \case
    Nothing -> throwIO $ UserException $ "Environment variable does not exist: " <> show envVar
    Just x -> case eitherDecode (BL.fromStrict $ encodeUtf8 $ toS x) of
      Left e -> throwIO $ UserException $ "Environment variable " <> show envVar <> " has invalid JSON: " <> show e
      Right r -> pure (key, r)

readEnvWithKey :: (Text, [Char]) -> IO (Text, Text)
readEnvWithKey (key, envVar) =
  lookupEnv envVar >>= \case
    Nothing -> throwIO $ UserException $ "Environment variable does not exist: " <> show envVar
    Just x -> pure (key, toS x)

envCompleter :: Optparse.Completer
envCompleter = Optparse.listIOCompleter do
  map fst <$> getEnvironment

readFileWithKey :: (Text, FilePath) -> IO (Text, Text)
readFileWithKey (key, file) = do
  bs <- BS.readFile file
  case decodeUtf8' bs of
    Left _e -> throwIO $ UserException $ "File " <> show file <> " for key " <> key <> " is not valid UTF-8."
    Right s -> pure (key, s)

readJsonFileWithKey :: FromJSON b => (Text, FilePath) -> IO (Text, b)
readJsonFileWithKey (key, file) = do
  bs <- BS.readFile file
  case eitherDecode (BL.fromStrict bs) of
    Left e -> throwIO $ UserException $ "File " <> show file <> " for key " <> key <> " is not valid JSON: " <> show e
    Right s -> pure (key, s)

readJsonFile :: FromJSON b => FilePath -> IO b
readJsonFile file = do
  bs <- BS.readFile file
  case eitherDecode (BL.fromStrict bs) of
    Left e -> throwIO $ UserException $ "File " <> show file <> " is not valid JSON: " <> show e
    Right s -> pure s

writeJsonFile :: ToJSON a => FilePath -> a -> IO ()
writeJsonFile filePath v =
  atomicWriteFile filePath $ BL.toStrict $ encodePretty' prettyConf v

printJson :: ToJSON a => a -> IO ()
printJson = BS.putStr . BL.toStrict . (<> "\n") . encodePretty' prettyConf

prettyConf :: Data.Aeson.Encode.Pretty.Config
prettyConf =
  defConfig
    { -- Indentation convention for Nix expressions is also 2
      confIndent = Spaces 2,
      -- UNIX convention
      confTrailingNewline = True
    }
