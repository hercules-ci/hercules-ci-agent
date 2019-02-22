{-# LANGUAGE DataKinds #-}
module Hercules.API.Projects where

import           Servant.API
import           Servant.API.Generic
import           Hercules.API.Attribute
import           Hercules.API.Result
import           Hercules.API.Prelude
import           Hercules.API.Projects.Job      ( Job )
import           Hercules.API.Projects.Project  ( Project )
import           Hercules.API.Accounts.Account  ( Account )
import           Hercules.API.Projects.CreateProject
                                                ( CreateProject )
import           Hercules.API.Derivation        ( Derivation )
import           Hercules.API.SourceHostingSite.SourceHostingSite
                                                ( SourceHostingSite )

data ProjectsAPI auth f = ProjectsAPI
  { projectsByOwner :: f :-
      Summary "List all projects owned by an account." :>
      "accounts" :>
      Capture' '[Required, Strict] "accountId" (Id Account) :>
      "projects" :>
      auth :>
      Get '[JSON] [Project]

  , findProjects :: f :-
      Summary "Find projects" :>
      "projects" :>
      QueryParam' '[Required] "site" (Name SourceHostingSite) :>
      QueryParam' '[Required] "account" (Name Account) :>
      QueryParam' '[Optional] "project" (Name Project) :>
      auth :>
      Get '[JSON] [Project]

  , createProject :: f :-
      Summary "Create a new project." :>
      "projects" :>
      auth :>
      ReqBody '[JSON] CreateProject :>
      Post '[JSON] (Id Project)

  , projectJobs :: f :-
      Summary "List all jobs in a project" :>
      Description "A list of a project's revisions and their details and status." :>
      "projects" :>
      Capture' '[Required, Strict] "project" (Id Project) :>
      "jobs" :>
      auth :>
      Get '[JSON] [Job]

  , findJobs :: f :-
      Summary "Find jobs" :>
      "jobs" :>
      QueryParam' '[Required] "site" (Name SourceHostingSite) :>
      QueryParam' '[Required] "account" (Name Account) :>
      QueryParam' '[Required] "project" (Name Project) :>
      QueryParam' '[Optional] "index" Int :>
      auth :>
      Get '[JSON] [Job]

  , projectJobAttributes :: f :-
      Summary "List all attributes in a job" :>
      Description "A list of all attributes that have been produced as part of the evaluation of a job." :>
      "jobs" :>
      Capture' '[Required, Strict] "jobId" (Id Job) :>
      "attributes" :>
      auth :>
      Get '[JSON] [Attribute (Result Text Derivation)]

  } deriving Generic
