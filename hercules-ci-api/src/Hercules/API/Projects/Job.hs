{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.Job
  ( module Hercules.API.Projects.Job,
    JobStatus (..),
    JobPhase (..),
  )
where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Evaluation.Evaluation
  ( Evaluation,
  )
import Hercules.API.Inputs.ImmutableInput (ImmutableInput)
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.Projects.SimpleJob (JobPhase (..), JobStatus (..))
import Hercules.API.Repos.Repo (Repo)

data Job = Job
  { id :: Id Job,
    projectId :: Id Project,
    index :: Int64,
    repoId :: Id Repo,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    jobPhase :: JobPhase,
    isCancelled :: Bool,
    jobStatus :: JobStatus,
    evaluationStatus :: JobStatus,
    derivationStatus :: JobStatus,
    effectsStatus :: JobStatus,
    evaluationId :: Id Evaluation,
    source :: GitCommitSource,
    -- | This is only correct when querying a single Job.
    extraInputs :: Map Text ImmutableInput,
    jobType :: JobType,
    jobName :: Maybe Text,
    rerunOf :: Maybe (Id Job),
    rerunOfIndex :: Maybe Int,
    startedBy :: Maybe (Id Account),
    cancelledBy :: Maybe (Id Account),
    mayCancel :: Bool,
    mayRerun :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data JobType
  = Config
  | Legacy
  | OnPush
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data GitCommitSource = GitCommitSource
  { revision :: Text,
    ref :: Text,
    message :: Text,
    gitCommitterName :: Text,
    committer :: Maybe Account,
    link :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data ProjectAndJobs = ProjectAndJobs
  { project :: Project,
    jobs :: [Job]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
