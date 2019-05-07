{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Projects.Job where

import           Hercules.API.Prelude

import           Hercules.API.Repos.Repo        ( Repo )
import           Hercules.API.Projects.Project  ( Project )
import           Hercules.API.Accounts.Account  ( Account )
import           Hercules.API.Evaluation.Evaluation
                                                ( Evaluation )

data Job = Job
  { id :: Id Job
  , projectId :: Id Project
  , index :: Int64
  , repoId :: Id Repo
  , startTime :: UTCTime
  , endTime :: Maybe UTCTime
  , jobPhase :: JobPhase
  , jobStatus :: JobStatus
  , evaluationStatus :: JobStatus
  , derivationStatus :: JobStatus
  , evaluationId :: Id Evaluation
  , source :: GitCommitSource
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)


data GitCommitSource = GitCommitSource
  { revision :: Text
  , ref :: Text
  , message :: Text
  , gitCommitterName :: Text
  , committerSlug :: Maybe (Name Account)
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

-- | Whichever is "worse": 'Failure' wins out, otherwise 'Pending' wins out, otherwise all are 'Success'.
instance Semigroup JobStatus where
  Failure <> _ = Failure
  _ <> Failure = Failure
  Pending <> _ = Pending
  _ <> Pending = Pending
  Success <> Success = Success

-- | @mappend@: Whichever is "worse": 'Failure' wins out, otherwise 'Pending' wins out, otherwise all are 'Success'.
--
-- @mempty@: 'Success'
instance Monoid JobStatus where
  mappend = (<>)
  mempty = Success

data JobAndProject = JobAndProject
  { project :: Project
  , job :: Job
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
