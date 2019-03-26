{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.Config where

import           Protolude
import qualified System.Environment
import qualified Dhall

data Config = Config
  { herculesApiBaseURL :: Text
  , clusterJoinTokenPath :: Text
  , concurrentTasks :: Integer
  } deriving (Generic, Dhall.Inject, Dhall.Interpret)

newDefaultConfig :: IO Config
newDefaultConfig = do
  baseUrl <- determineDefaultApiBaseUrl
  pure Config
    { herculesApiBaseURL = baseUrl
    , clusterJoinTokenPath = panic "Config.clusterJoinTokenPath wasn't set."
    , concurrentTasks = 4
    }

determineDefaultApiBaseUrl :: IO Text
determineDefaultApiBaseUrl = do
  maybeEnv <- System.Environment.lookupEnv "HERCULES_API_BASE_URL"
  pure $ maybe defaultApiBaseUrl toS maybeEnv

defaultApiBaseUrl :: Text
defaultApiBaseUrl = "https://hercules-ci.com"

readConfig :: Maybe Text -> IO Config
readConfig loc = do
  case loc of
    Just x -> Dhall.input Dhall.auto $ toS x
    Nothing -> newDefaultConfig
