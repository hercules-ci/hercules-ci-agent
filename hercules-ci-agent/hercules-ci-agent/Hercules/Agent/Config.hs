{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Hercules.Agent.Config
  ( Config (..),
    FinalConfig,
    ConfigPath (..),
    Purpose (..),
    readConfig,
    finalizeConfig,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import qualified Data.Vector as V
import GHC.Conc (getNumProcessors)
import Hercules.CNix.Verbosity (Verbosity (..))
import Katip (Severity (..))
import Protolude hiding (to)
import qualified System.Environment
import System.FilePath ((</>))
import Toml

data ConfigPath = TomlPath FilePath

nounPhrase :: ConfigPath -> Text
nounPhrase (TomlPath p) = "your agent.toml file from " <> show p

data Purpose = Input | Final

-- | Whether the 'Final' value is optional.
data Sort = Required | Optional | From Sort Type

type family Item purpose sort a where
  Item 'Input ('From sort b) a = Item 'Input sort b
  Item 'Final ('From sort b) a = Item 'Final sort a
  Item 'Input _sort a = Maybe a
  Item 'Final 'Required a = a
  Item 'Final 'Optional a = Maybe a

type FinalConfig = Config 'Final

data Config purpose = Config
  { herculesApiBaseURL :: Item purpose 'Required Text,
    nixUserIsTrusted :: Item purpose 'Required Bool,
    concurrentTasks :: Item purpose ('From 'Required (Either () Int)) Int,
    baseDirectory :: Item purpose 'Required FilePath,
    -- | Read-only
    staticSecretsDirectory :: Item purpose 'Required FilePath,
    workDirectory :: Item purpose 'Required FilePath,
    clusterJoinTokenPath :: Item purpose 'Required FilePath,
    binaryCachesPath :: Item purpose 'Required FilePath,
    secretsJsonPath :: Item purpose 'Required FilePath,
    logLevel :: Item purpose 'Required Severity,
    nixVerbosity :: Item purpose 'Required Verbosity,
    labels :: Item purpose 'Required (Map Text A.Value),
    allowInsecureBuiltinFetchers :: Item purpose 'Required Bool
  }
  deriving (Generic)

deriving instance Show (Config 'Final)

tomlCodec :: TomlCodec (Config 'Input)
tomlCodec =
  Config
    <$> dioptional (Toml.text "apiBaseUrl")
    .= herculesApiBaseURL
    <*> dioptional (Toml.bool "nixUserIsTrusted")
    .= nixUserIsTrusted
    <*> dioptional
      ( Toml.dimatch matchRight Right (Toml.int "concurrentTasks")
          <|> Toml.dimatch matchLeft Left (Toml.textBy (\() -> "auto") isAuto "concurrentTasks")
      )
    .= concurrentTasks
    <*> dioptional (Toml.string keyBaseDirectory)
    .= baseDirectory
    <*> dioptional (Toml.string "staticSecretsDirectory")
    .= staticSecretsDirectory
    <*> dioptional (Toml.string "workDirectory")
    .= workDirectory
    <*> dioptional (Toml.string keyClusterJoinTokenPath)
    .= clusterJoinTokenPath
    <*> dioptional (Toml.string "binaryCachesPath")
    .= binaryCachesPath
    <*> dioptional (Toml.string "secretsJsonPath")
    .= secretsJsonPath
    <*> dioptional (Toml.enumBounded "logLevel")
    .= logLevel
    <*> dioptional (Toml.enumBounded "nixVerbosity")
    .= nixVerbosity
    <*> dioptional (Toml.tableMap _KeyText embedJson "labels")
    .= labels
    <*> dioptional (Toml.bool "allowInsecureBuiltinFetchers")
    .= allowInsecureBuiltinFetchers

embedJson :: Key -> TomlCodec A.Value
embedJson key =
  Codec
    { codecRead =
        codecRead (match (embedJsonBiMap key) key)
          <!> codecRead (A.Object . AK.fromHashMapText <$> Toml.tableHashMap _KeyText embedJson key),
      codecWrite = panic "embedJson.write: not implemented" $ \case
        A.String s -> A.String <$> codecWrite (Toml.text key) s
        A.Number sci -> A.Number . fromRational . toRational <$> codecWrite (Toml.double key) (fromRational $ toRational sci)
        A.Bool b -> A.Bool <$> codecWrite (Toml.bool key) b
        A.Array a -> A.Array . V.fromList <$> codecWrite (Toml.arrayOf (embedJsonBiMap key) key) (Protolude.toList a)
        A.Object o -> A.Object . AK.fromHashMapText <$> codecWrite (Toml.tableHashMap _KeyText embedJson key) (AK.toHashMapText o)
        A.Null -> eitherToTomlState (Left ("null is not supported in TOML" :: Text))
    }

embedJsonBiMap :: Key -> TomlBiMap A.Value AnyValue
embedJsonBiMap _key =
  -- TODO: use key for error reporting
  BiMap
    { forward = panic "embedJsonBiMap.forward: not implemented" $ \case
        A.String s -> pure $ AnyValue $ Text s
        A.Number sci -> case floatingOrInteger sci of
          Left fl -> pure $ AnyValue $ Double fl -- lossy
          Right i -> pure $ AnyValue $ Integer i
        A.Bool b -> pure $ AnyValue $ Bool b
        A.Array _a -> Left $ ArbitraryError "Conversion from JSON array of arrays to TOML not implemented yet"
        A.Object _o -> Left $ ArbitraryError "Conversion from JSON array of objects to TOML is not supported"
        A.Null -> Left $ ArbitraryError "JSON null is not supported in TOML",
      backward = anyValueToJSON
    }

anyValueToJSON :: AnyValue -> Either TomlBiMapError A.Value
anyValueToJSON = \case
  AnyValue (Bool b) -> pure (A.Bool b)
  AnyValue (Integer i) -> pure (A.Number $ fromIntegral i)
  AnyValue (Double d) -> pure (A.Number $ fromFloatDigits d)
  AnyValue (Text t) -> pure (A.String t)
  AnyValue (Zoned _zt) -> Left (ArbitraryError "Conversion from TOML zoned time to JSON not implemented yet. Use a string.")
  AnyValue (Local _zt) -> Left (ArbitraryError "Conversion from TOML local time to JSON not implemented yet. Use a string.")
  AnyValue (Day _d) -> Left (ArbitraryError "Conversion from TOML day to JSON not implemented yet. Use a string.")
  AnyValue (Hours _h) -> Left (ArbitraryError "Conversion from TOML hours to JSON not implemented yet. Use a string.")
  AnyValue (Array a) -> A.Array <$> sequence (V.fromList (a <&> AnyValue <&> anyValueToJSON))

matchLeft :: Either a b -> Maybe a
matchLeft (Left a) = Just a
matchLeft _ = Nothing

matchRight :: Either a1 a2 -> Maybe a2
matchRight (Right a) = Just a
matchRight _ = Nothing

isAuto :: Text -> Either Text ()
isAuto "auto" = Right ()
isAuto _ = Left "The only permissible string value is \"auto\""

keyClusterJoinTokenPath :: Key
keyClusterJoinTokenPath = "clusterJoinTokenPath"

keyBaseDirectory :: Key
keyBaseDirectory = "baseDirectory"

determineDefaultApiBaseUrl :: IO Text
determineDefaultApiBaseUrl = do
  maybeEnv <- System.Environment.lookupEnv "HERCULES_CI_API_BASE_URL"
  maybeEnv' <- System.Environment.lookupEnv "HERCULES_API_BASE_URL"
  pure $ maybe defaultApiBaseUrl toS (maybeEnv <|> maybeEnv')

defaultApiBaseUrl :: Text
defaultApiBaseUrl = "https://hercules-ci.com"

readConfig :: ConfigPath -> IO (Config 'Input)
readConfig loc = case loc of
  TomlPath fp -> Toml.decodeFile tomlCodec (toS fp)

finalizeConfig :: ConfigPath -> Config 'Input -> IO (Config 'Final)
finalizeConfig loc input = do
  baseDir <-
    case baseDirectory input of
      Just x -> pure x
      Nothing -> throwIO $ FatalError $ "You need to specify " <> show keyBaseDirectory <> " in " <> nounPhrase loc
  let staticSecretsDir =
        fromMaybe (baseDir </> "secrets") (staticSecretsDirectory input)
      clusterJoinTokenP =
        fromMaybe
          (staticSecretsDir </> "cluster-join-token.key")
          (clusterJoinTokenPath input)
      binaryCachesP =
        fromMaybe
          (staticSecretsDir </> "binary-caches.json")
          (binaryCachesPath input)
      secretsJsonP =
        fromMaybe
          (staticSecretsDir </> "secrets.json")
          (secretsJsonPath input)
      workDir = fromMaybe (baseDir </> "work") (workDirectory input)
  dabu <- determineDefaultApiBaseUrl
  numProc <- getNumProcessors
  let -- make sure the default is at least two, for ifd
      autoConcurrentTasks = max 2 numProc
      configuredConcurrentTasks = case concurrentTasks input of
        Nothing -> autoConcurrentTasks
        Just (Left _auto) -> autoConcurrentTasks
        Just (Right n) -> fromIntegral n
  validConcurrentTasks <-
    case configuredConcurrentTasks of
      x | x >= 1 -> pure x
      _ -> throwIO $ FatalError "concurrentTasks must be at least 1"
  let apiBaseUrl = fromMaybe dabu $ herculesApiBaseURL input
  pure
    Config
      { herculesApiBaseURL = apiBaseUrl,
        nixUserIsTrusted = fromMaybe False $ nixUserIsTrusted input,
        binaryCachesPath = binaryCachesP,
        clusterJoinTokenPath = clusterJoinTokenP,
        concurrentTasks = validConcurrentTasks,
        baseDirectory = baseDir,
        staticSecretsDirectory = staticSecretsDir,
        secretsJsonPath = secretsJsonP,
        workDirectory = workDir,
        logLevel = logLevel input & fromMaybe InfoS,
        nixVerbosity = nixVerbosity input & fromMaybe Talkative,
        labels = fromMaybe mempty $ labels input,
        allowInsecureBuiltinFetchers = fromMaybe False $ allowInsecureBuiltinFetchers input
      }
