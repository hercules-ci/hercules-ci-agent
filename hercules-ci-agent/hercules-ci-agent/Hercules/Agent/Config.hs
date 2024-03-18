{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.Config
  ( Config (..),
    FinalConfig,
    ConfigPath (..),
    Purpose (..),
    Mountable (..),
    readConfig,
    finalizeConfig,

    -- * Export for testing
    combiCodec,
  )
where

import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Profunctor (Star (Star))
import GHC.Conc (getNumProcessors)
import Hercules.Agent.Config.Combined
import Hercules.Agent.Config.Json (GCodec (GCodec), (.=.))
import Hercules.Agent.Config.Json qualified as Json
import Hercules.Agent.Config.Toml qualified as Toml
import Hercules.CNix.Verbosity (Verbosity (..))
import Hercules.Formats.Mountable (Mountable (Mountable))
import Hercules.Formats.Mountable qualified as Mountable
import Katip (Severity (..))
import Protolude hiding (to)
import System.Environment qualified
import System.FilePath ((</>))
import Toml (Key)
import Toml qualified

newtype ConfigPath = ConfigPath FilePath

nounPhrase :: ConfigPath -> Text
nounPhrase (ConfigPath p) = "your agent config file from " <> show p

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
    allowInsecureBuiltinFetchers :: Item purpose 'Required Bool,
    remotePlatformsWithSameFeatures :: Item purpose 'Optional [Text],
    effectMountables :: Map Text Mountable,
    nixSettings :: Map Text Text
  }
  deriving (Generic)

deriving instance Show (Config 'Final)

combiCodec :: Combi' (Config 'Input)
combiCodec =
  Config
    <$> opt (textAtKey "apiBaseUrl") .=. herculesApiBaseURL
    <*> opt (boolAtKey "nixUserIsTrusted") .=. nixUserIsTrusted
    <*> opt
      ( combi
          ( Toml.dimatch matchRight Right (Toml.int "concurrentTasks")
              <|> Toml.dimatch matchLeft Left (Toml.textBy (\() -> "auto") isAuto "concurrentTasks")
          )
          ( Json.dimatch matchRight Right (Json.int "concurrentTasks")
              <|> Json.dimatch matchLeft Left (Json.textBy (\() -> "auto") isAuto "concurrentTasks")
          )
      )
      .=. concurrentTasks
    <*> opt (stringAtKey keyBaseDirectory) .=. baseDirectory
    <*> opt (stringAtKey "staticSecretsDirectory") .=. staticSecretsDirectory
    <*> opt (stringAtKey "workDirectory") .=. workDirectory
    <*> opt (stringAtKey keyClusterJoinTokenPath) .=. clusterJoinTokenPath
    <*> opt (stringAtKey "binaryCachesPath") .=. binaryCachesPath
    <*> opt (stringAtKey "secretsJsonPath") .=. secretsJsonPath
    <*> opt (enumBoundedAtKey "logLevel") .=. logLevel
    <*> opt (enumBoundedAtKey "nixVerbosity") .=. nixVerbosity
    <*> opt
      ( combi
          (Toml.tableMap Toml._KeyText Toml.embedJson "labels")
          (Json.tableMap' Json.value' "labels")
      )
      .=. labels
    <*> opt (boolAtKey "allowInsecureBuiltinFetchers") .=. allowInsecureBuiltinFetchers
    <*> opt
      ( combi
          (Toml.arrayOf Toml._Text "remotePlatformsWithSameFeatures")
          (Json.arrayOf Json._Text "remotePlatformsWithSameFeatures")
      )
      .=. remotePlatformsWithSameFeatures
    <*> optEmpty
      ( combi
          (Toml.tableMap Toml._KeyText (Toml.table (forToml mountableCodec)) "effectMountables")
          (Json.tableMap' (forJson mountableCodec) "effectMountables")
      )
      .=. effectMountables
    <*> optEmpty
      (tableMap textAtKey "nixSettings")
      .=. nixSettings

mountableCodec :: Combi' Mountable
mountableCodec =
  Mountable
    <$> textAtKey "source" .=. Mountable.source
    <*> boolAtKey "readOnly" .=. Mountable.readOnly
    <*> combi
      ((throwImmediately . A.parseJSON) <$> Toml.embedJson "condition")
      ( GCodec
          ( do
              let parse =
                    A.withObject "Mountable" $ \o ->
                      o A..: "condition"
              v <- ask
              -- seq (Debug.Trace.traceShowId $ Debug.Trace.trace "------------" v) pass
              case A.parseEither parse v of
                Right x -> pure x
                Left e -> throwError $ "Error parsing condition for mountable: " <> show e
          )
          (Star $ \_ -> panic "condition toJSON not implemented")
      )
      .=. (A.toJSON . Mountable.condition)
  where
    -- "Parser" is more like "parse result"
    -- TODO: more friendly reporting?
    throwImmediately :: A.Parser a -> a
    throwImmediately p =
      A.parseEither (const p) () & \case
        Left e -> panic $ "Error parsing condition for mountable: " <> show e
        Right a -> a

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
  ConfigPath fp | ".json" `isSuffixOf` fp -> Json.decodeFile (forJson combiCodec) (toS fp)
  ConfigPath fp -> Toml.decodeFile (forToml combiCodec) (toS fp)

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
        allowInsecureBuiltinFetchers = fromMaybe False $ allowInsecureBuiltinFetchers input,
        remotePlatformsWithSameFeatures = remotePlatformsWithSameFeatures input,
        effectMountables = effectMountables input,
        nixSettings = nixSettings input
      }
