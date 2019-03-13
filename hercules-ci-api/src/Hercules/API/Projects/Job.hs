{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Projects.Job where

import           Hercules.API.Prelude

import           Hercules.API.Repos.Repo        ( Repo )
import           Hercules.API.Projects.Project  ( Project )
import           Hercules.API.Evaluation.Evaluation
                                                ( Evaluation )

data Job = Job
  { id :: Id Job
  , projectId :: Id Project
  , index :: Int64
  , repoId :: Id Repo
  , revision :: Text
  , startTime :: UTCTime
  , endTime :: Maybe UTCTime
  , jobPhase :: JobPhase
  , jobStatus :: JobStatus
  , evaluationStatus :: JobStatus
  , derivationStatus :: JobStatus
  , evaluation :: Evaluation
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data JobPhase
  = Queued
  | Evaluating
  | Building
  | Done
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data JobStatus
  = Pending
  | Failure
  | Success
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data Contextual context a = Contextual
  { context :: context
  , value :: a
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
deriving instance (ToSchema context, ToSchema a) => ToSchema (Contextual context a)
