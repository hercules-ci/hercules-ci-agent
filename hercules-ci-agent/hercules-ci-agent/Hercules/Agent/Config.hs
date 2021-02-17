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

import GHC.Conc (getNumProcessors)
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
    logLevel :: Item purpose 'Required Severity
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
    <*> dioptional (Toml.enumBounded "logLevel")
    .= logLevel

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
        workDirectory = workDir,
        logLevel = logLevel input & fromMaybe InfoS
      }
