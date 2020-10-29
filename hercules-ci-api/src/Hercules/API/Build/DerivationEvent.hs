{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hercules.API.Build.DerivationEvent where

import Data.Aeson.Types (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Hercules.API.Build.DerivationEvent.BuiltOutput
import Hercules.API.Prelude
import Hercules.API.Projects.SimpleJob (SimpleJob)

data DerivationEvent
  = Queued DerivationEventQueued
  | DependencyFailed DerivationEventDependencyFailed
  | Started DerivationEventStarted
  | Reset DerivationEventReset
  | Failed DerivationEventFailed
  | Succeeded DerivationEventSucceeded
  | Cancelled DerivationEventCancelled
  | Built DerivationEventBuilt
  deriving (Generic, Show, Eq, NFData, ToSchema)

instance FromJSON DerivationEvent where
  parseJSON = genericParseJSON schemaCompatibleOptions

instance ToJSON DerivationEvent where
  toJSON = genericToJSON schemaCompatibleOptions

  toEncoding = genericToEncoding schemaCompatibleOptions

eventTime :: DerivationEvent -> UTCTime
eventTime (Queued (DerivationEventQueued {time = t})) = t
eventTime (DependencyFailed (DerivationEventDependencyFailed {time = t})) = t
eventTime (Started (DerivationEventStarted {time = t})) = t
eventTime (Reset (DerivationEventReset {time = t})) = t
eventTime (Failed (DerivationEventFailed {time = t})) = t
eventTime (Succeeded (DerivationEventSucceeded {time = t})) = t
eventTime (Cancelled (DerivationEventCancelled {time = t})) = t
eventTime (Built (DerivationEventBuilt {time = t})) = t

data DerivationEventQueued = DerivationEventQueued
  { time :: UTCTime,
    requeuedForEvalOfJob :: Maybe SimpleJob,
    requeuedForAgent :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventDependencyFailed = DerivationEventDependencyFailed
  { time :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventStarted = DerivationEventStarted
  { time :: UTCTime,
    logId :: Id "log",
    agentHostname :: Maybe Text,
    streamable :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventReset = DerivationEventReset
  { time :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventFailed = DerivationEventFailed
  { time :: UTCTime,
    technicalError :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventSucceeded = DerivationEventSucceeded
  { time :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventCancelled = DerivationEventCancelled
  { time :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationEventBuilt = DerivationEventBuilt
  { time :: UTCTime,
    outputs :: [BuiltOutput]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
