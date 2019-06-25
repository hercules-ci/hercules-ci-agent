{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Hercules.Agent.Config
  ( Config(..)
  , ConfigPath(..)
  , Purpose(..)
  , readConfig
  , finalizeConfig
  )
where

import           Protolude               hiding ( to )
import qualified System.Environment
import           Hercules.Error
import           Toml

data ConfigPath = TomlPath FilePath

data Purpose = Input | Final

-- | Whether the 'Final' value is optional.
data Sort = Required | Optional
type family Item purpose sort a where
  Item 'Input _sort a = Maybe a
  Item 'Final 'Required a = a
  Item 'Final 'Optional a = Maybe a

data Config purpose = Config
  { herculesApiBaseURL :: Item purpose 'Required Text
  , clusterJoinTokenPath :: Item purpose 'Required Text
  , concurrentTasks :: Item purpose 'Required Integer
  , binaryCachesPath :: Item purpose 'Optional Text
  } deriving (Generic)
deriving instance Show (Config 'Final)

tomlCodec :: TomlCodec (Config 'Input)
tomlCodec =
  Config
    <$> dioptional (Toml.text "apiBaseUrl")
    .= herculesApiBaseURL
    <*> dioptional (Toml.text keyClusterJoinTokenPath)
    .= clusterJoinTokenPath
    <*> dioptional (Toml.integer "concurrentTasks")
    .= concurrentTasks
    <*> dioptional (Toml.text "binaryCachesPath")
    .= binaryCachesPath

keyClusterJoinTokenPath :: Key
keyClusterJoinTokenPath = "clusterJoinTokenPath"

determineDefaultApiBaseUrl :: IO Text
determineDefaultApiBaseUrl = do
  maybeEnv <- System.Environment.lookupEnv "HERCULES_API_BASE_URL"
  pure $ maybe defaultApiBaseUrl toS maybeEnv

defaultApiBaseUrl :: Text
defaultApiBaseUrl = "https://hercules-ci.com"

defaultConcurrentTasks :: Integer
defaultConcurrentTasks = 4

readConfig :: ConfigPath -> IO (Config 'Input)
readConfig loc = case loc of
  TomlPath fp -> Toml.decodeFile tomlCodec (toSL fp)

finalizeConfig :: Config 'Input -> IO (Config 'Final)
finalizeConfig input = do
  dabu <- determineDefaultApiBaseUrl
  cjtp <- escalate $ maybeToEither
    (FatalError
    $ "You need to specify key "
    <> show keyClusterJoinTokenPath
    <> " in your configuration file"
    )
    (clusterJoinTokenPath input)
  pure Config
    { herculesApiBaseURL = fromMaybe dabu $ herculesApiBaseURL input
    , binaryCachesPath = binaryCachesPath input
    , clusterJoinTokenPath = cjtp
    , concurrentTasks = fromMaybe defaultConcurrentTasks $ concurrentTasks input
    }
