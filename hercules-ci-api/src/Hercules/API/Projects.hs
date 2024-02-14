{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects where

import Data.OpenApi qualified as O3
import GHC.Records (HasField (getField))
import Hercules.API.Accounts.Account (Account)
import Hercules.API.Build.EvaluationDetail
  ( EvaluationDetail,
  )
import Hercules.API.Build.EvaluationDiff (EvaluationDiff)
import Hercules.API.Build.FailureGraph qualified as FailureGraph
import Hercules.API.Build.Log (Log)
import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Inputs.ImmutableGitInput (ImmutableGitInput)
import Hercules.API.Paging (PagedResponse)
import Hercules.API.Prelude
import Hercules.API.Projects.CreateProject
  ( CreateProject,
  )
import Hercules.API.Projects.CreateUserEffectTokenResponse (CreateUserEffectTokenResponse)
import Hercules.API.Projects.Job
  ( Job,
    JobType,
    ProjectAndJobs,
  )
import Hercules.API.Projects.JobHandlers (JobHandlers)
import Hercules.API.Projects.PatchProject
  ( PatchProject,
  )
import Hercules.API.Projects.Project (Project)
import Servant.API

type GetJsonWithPreflight a =
  Get
    '[JSON]
    (Headers '[Header "Access-Control-Allow-Origin" Text] a)
    :<|> Verb
           'OPTIONS
           204
           '[JSON]
           ( Headers
               '[ Header "Access-Control-Allow-Origin" Text,
                  Header "Access-Control-Allow-Headers" Text,
                  Header "Access-Control-Allow-Methods" Text
                ]
               NoContent
           )

data ProjectResourceGroup auth f = ProjectResourceGroup
  { get ::
      f
        :- Summary "Retrieve a project"
          :> auth
          :> Get '[JSON] Project,
    getJobs ::
      f
        :- Summary "Retrieve information about jobs"
          :> "jobs"
          :> QueryParam' '[Optional, Description "Constrain the results by git ref, such as refs/heads/my-branch or HEAD"] "ref" Text
          :> QueryParam' '[Optional, Description "Git commit hash (from which to load the job handler definition)"] "rev" (Name "Rev")
          :> QueryParam' '[Optional, Description "Job handler type, such as onPush or onSchedule"] "handler" JobType
          :> QueryParam' '[Optional, Description "Job handler name, such as <name> in onPush.<name>"] "name" (Name "JobName")
          :> QueryParam' '[Optional, Description "Only return successful jobs, or only failed ones"] "success" Bool
          :> QueryParam' '[Optional, Description "Return jobs that come \"after\" the provided id in the response order."] "offsetId" (Id Job)
          :> QueryParam' '[Optional, Description "Return jobs that come \"after\" the provided index in the response order."] "offsetIndex" Int64
          :> QueryParam' '[Optional, Description "Return at most n jobs."] "limit" Int64
          :> auth
          :> GetJsonWithPreflight PagedJobs,
    getJobSource ::
      f
        :- Summary "Get source information from the latest successful job/jobs satisfying the provided requirements."
          :> Description "The job parameter can be omitted to require all jobs for a commit to succeed. This can have the unexpected effect of reverting when a change in the extraInputs causes a regression. So it is recommended to specify one or more jobs. Common examples are \"onPush.default\" for a pinned build or \"onPush.ci\" for a build using extraInputs to integrate continuously."
          :> "source"
          :> QueryParam' '[Optional, Description "Constrain the results by git ref, such as refs/heads/my-branch. Defaults to HEAD."] "ref" Text
          :> QueryParams "jobs" Text
          :> auth
          :> Get '[JSON] ImmutableGitInput
  }
  deriving (Generic)

data ProjectsAPI auth f = ProjectsAPI
  { byProjectId ::
      f
        :- Substitute
             ( "projects"
                 :> Capture' '[Required, Strict] "projectId" (Id Project)
                 :> Placeholder
             )
             (ToServantApi (ProjectResourceGroup auth)),
    byProjectName ::
      f
        :- Substitute
             ( "site"
                 :> Capture' '[Required, Strict] "site" (Name Forge)
                 :> "account"
                 :> Capture' '[Required, Strict] "account" (Name Account)
                 :> "project"
                 :> Capture' '[Required, Strict] "project" (Name Project)
                 :> Placeholder
             )
             (ToServantApi (ProjectResourceGroup auth)),
    projectsByOwner ::
      f
        :- Summary "List all projects owned by an account."
          :> "accounts"
          :> Capture' '[Required, Strict] "accountId" (Id Account)
          :> "projects"
          :> auth
          :> Get '[JSON] [Project],
    findProjects ::
      f
        :- Summary "Find projects"
          :> "projects"
          :> QueryParam' '[Optional] "site" (Name Forge)
          :> QueryParam' '[Optional] "account" (Name Account)
          :> QueryParam' '[Optional] "project" (Name Project)
          :> auth
          :> Get '[JSON] [Project],
    createProject ::
      f
        :- Summary "Create a new project."
          :> "projects"
          :> auth
          :> ReqBody '[JSON] CreateProject
          :> Post '[JSON] (Id Project),
    patchProject ::
      f
        :- Summary "Modify a project"
          :> "projects"
          :> Capture' '[Required, Strict] "projectId" (Id Project)
          :> ReqBody '[JSON] PatchProject
          :> auth
          :> Patch '[JSON] Project,
    createUserEffectToken ::
      f
        :- Summary "Create a token for local effect execution"
          :> "projects"
          :> Capture' '[Required, Strict] "projectId" (Id Project)
          :> auth
          :> "create-user-effect-token"
          :> Post '[JSON] CreateUserEffectTokenResponse,
    findJobs ::
      f
        :- Summary "Find jobs in multiple projects at once"
          :> Description "For a more powerful single project endpoint, see /api/v1/site/{site}/account/{account}/project/{project}/jobs"
          :> "jobs"
          :> QueryParam' '[Optional, Description "Currently only \"github\" or omit entirely"] "site" (Name Forge)
          :> QueryParam' '[Optional, Description "Account name filter"] "account" (Name Account)
          :> QueryParam' '[Optional, Description "Project name filter. Required if you want to retrieve all jobs"] "project" (Name Project)
          :> QueryParam' '[Optional, Description "To get a specific job by index"] "index" Int
          :> QueryParam' '[Optional, Description "Number of latest jobs to get, when project name is omitted. Range [1..50], default 10."] "latest" Int
          :> auth
          :> Get '[JSON] [ProjectAndJobs],
    getJob ::
      f
        :- Summary "Retrieve a job"
          :> Description "Retrieve a job"
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> auth
          :> Get '[JSON] Job,
    getJobHandlers ::
      f
        :- Summary "Get a job's handler declarations, if any."
          :> Description "Handlers define what to build and do on events such as onPush, onSchedule."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "handlers"
          :> auth
          :> Get '[JSON] JobHandlers,
    projectJobEvaluation ::
      f
        :- Summary "List all attributes in a job"
          :> Description "A list of all attributes that have been produced as part of the evaluation of a job."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "evaluation"
          :> auth
          :> GetJsonWithPreflight EvaluationDetail,
    projectJobEvaluationDiff ::
      f
        :- Summary "Compare two evaluations"
          :> Description "A list of attributes that have been added, removed or changed between two evaluations. Also lists changes to the IFD derivations."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "evaluation"
          :> "compare"
          :> Capture' '[Required, Strict] "baseJobId" (Id Job)
          :> auth
          :> GetJsonWithPreflight EvaluationDiff,
    jobDerivationFailureGraph ::
      f
        :- Summary "Find all failures in an evaluation's derivations"
          :> Description
               "Returns all derivations that have failures in their dependency closures."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "derivations"
          :> "failed"
          :> auth
          :> Get '[JSON] FailureGraph.Graph,
    jobRerun ::
      f
        :- Summary "Create a new job like this job"
          :> Description
               "The newly created job will be in the same project, have the same inputs but a new evaluation.\
               \ The response has the newly created job."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "rerun"
          :> QueryParam "rebuildFailures" Bool
          :> auth
          :> Post '[JSON] Job,
    jobTriggerOnSchedule ::
      f
        :- Summary "Create a scheduled job to run now, based on a configuration job."
          :> Description
               "This is mostly intended for trying out new scheduled jobs before they are merged. The job is run in the context of the job's branch; not that of the default branch."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "on-schedule"
          :> Capture' '[Required, Strict] "jobName" Text
          :> "run"
          :> auth
          :> Post '[JSON] Job,
    jobCancel ::
      f
        :- Summary "Cancel the job and any work that becomes redundant"
          :> Description
               "Some derivations may keep going, if referenced by active jobs."
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "cancel"
          :> auth
          :> Post '[JSON] NoContent,
    getEvaluationLog ::
      f
        :- Summary "Read all recorded evaluation log entries"
          :> "jobs"
          :> Capture' '[Required, Strict] "jobId" (Id Job)
          :> "evaluation"
          :> "log"
          :> "lines"
          :> QueryParam' '[Required] "logId" (Id "log")
          :> QueryParam' '[Optional] "iMin" Int
          :> auth
          :> Get '[JSON] Log
  }
  deriving (Generic)

newtype PagedJobs = PagedJobs (PagedResponse Job)
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

instance HasField "get" PagedJobs (PagedResponse Job) where getField (PagedJobs a) = a
