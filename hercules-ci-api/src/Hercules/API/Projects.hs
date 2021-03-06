{-# LANGUAGE DataKinds #-}

module Hercules.API.Projects where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Build.EvaluationDetail
  ( EvaluationDetail,
  )
import qualified Hercules.API.Build.FailureGraph as FailureGraph
import Hercules.API.Build.Log (Log)
import Hercules.API.Prelude
import Hercules.API.Projects.CreateProject
  ( CreateProject,
  )
import Hercules.API.Projects.CreateUserEffectTokenResponse (CreateUserEffectTokenResponse)
import Hercules.API.Projects.Job
  ( Job,
    ProjectAndJobs,
  )
import Hercules.API.Projects.PatchProject
  ( PatchProject,
  )
import Hercules.API.Projects.Project (Project)
import Hercules.API.SourceHostingSite.SourceHostingSite
  ( SourceHostingSite,
  )
import Servant.API
import Servant.API.Generic

data ProjectsAPI auth f = ProjectsAPI
  { projectsByOwner ::
      f :- Summary "List all projects owned by an account."
        :> "accounts"
        :> Capture' '[Required, Strict] "accountId" (Id Account)
        :> "projects"
        :> auth
        :> Get '[JSON] [Project],
    findProjects ::
      f :- Summary "Find projects"
        :> "projects"
        :> QueryParam' '[Optional] "site" (Name SourceHostingSite)
        :> QueryParam' '[Optional] "account" (Name Account)
        :> QueryParam' '[Optional] "project" (Name Project)
        :> auth
        :> Get '[JSON] [Project],
    createProject ::
      f :- Summary "Create a new project."
        :> "projects"
        :> auth
        :> ReqBody '[JSON] CreateProject
        :> Post '[JSON] (Id Project),
    patchProject ::
      f :- Summary "Modify a project"
        :> "projects"
        :> Capture' '[Required, Strict] "projectId" (Id Project)
        :> ReqBody '[JSON] PatchProject
        :> auth
        :> Patch '[JSON] Project,
    createUserEffectToken ::
      f :- Summary "Create a token for local effect execution"
        :> "projects"
        :> Capture' '[Required, Strict] "projectId" (Id Project)
        :> auth
        :> "create-user-effect-token"
        :> Post '[JSON] CreateUserEffectTokenResponse,
    projectJobs ::
      f :- Summary "List all jobs in a project"
        :> Description "A list of a project's revisions and their details and status."
        :> "projects"
        :> Capture' '[Required, Strict] "project" (Id Project)
        :> "jobs"
        :> auth
        :> Get '[JSON] [Job],
    findJobs ::
      f :- Summary "Find jobs"
        :> "jobs"
        :> QueryParam' '[Optional, Description "Currently only \"github\" or omit entirely"] "site" (Name SourceHostingSite)
        :> QueryParam' '[Optional, Description "Account name filter"] "account" (Name Account)
        :> QueryParam' '[Optional, Description "Project name filter. Required if you want to retrieve all jobs"] "project" (Name Project)
        :> QueryParam' '[Optional, Description "To get a specific job by index"] "index" Int
        :> QueryParam' '[Optional, Description "Number of latest jobs to get, when project name is omitted. Range [1..50], default 10."] "latest" Int
        :> auth
        :> Get '[JSON] [ProjectAndJobs],
    projectJobEvaluation ::
      f :- Summary "List all attributes in a job"
        :> Description "A list of all attributes that have been produced as part of the evaluation of a job."
        :> "jobs"
        :> Capture' '[Required, Strict] "jobId" (Id Job)
        :> "evaluation"
        :> auth
        :> Get '[JSON] EvaluationDetail,
    jobDerivationFailureGraph ::
      f :- Summary "Find all failures in an evaluation's derivations"
        :> Description
             "Returns all derivations that have failures in their dependency closures."
        :> "jobs"
        :> Capture' '[Required, Strict] "jobId" (Id Job)
        :> "derivations"
        :> "failed"
        :> auth
        :> Get '[JSON] FailureGraph.Graph,
    jobRerun ::
      f :- Summary "Create a new job like this job"
        :> Description
             "The newly created job will be in the same project, have the same inputs but a new evaluation.\
             \ The response has the newly created job."
        :> "jobs"
        :> Capture' '[Required, Strict] "jobId" (Id Job)
        :> "rerun"
        :> QueryParam "rebuildFailures" Bool
        :> auth
        :> Post '[JSON] Job,
    jobCancel ::
      f :- Summary "Cancel the job and any work that becomes redundant"
        :> Description
             "Some derivations may keep going, if referenced by active jobs."
        :> "jobs"
        :> Capture' '[Required, Strict] "jobId" (Id Job)
        :> "cancel"
        :> auth
        :> Post '[JSON] NoContent,
    getEvaluationLog ::
      f :- Summary "Read all recorded evaluation log entries"
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
