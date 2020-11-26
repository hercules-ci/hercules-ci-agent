{-# LANGUAGE DataKinds #-}

module Hercules.API.Effects where

import Hercules.API.Attribute
import Hercules.API.Build.Log (Log)
import Hercules.API.Effects.EffectInfo
import Hercules.API.Prelude
import Hercules.API.Projects.Job (Job)
import Servant.API (Capture, Get, JSON, NoContent, Optional, Post, QueryParam', Required, Summary, (:>))
import Servant.API.Generic (GenericMode ((:-)))

data EffectsAPI auth f = EffectsAPI
  { getEffect ::
      f :- Summary "Read effect events"
        :> "jobs"
        :> Capture "jobId" (Id Job)
        :> "effects"
        :> Capture "attribute" AttributePath
        :> auth
        :> Get '[JSON] EffectInfo,
    getEffectLog ::
      f :- Summary "Read all recorded log entries"
        :> "jobs"
        :> Capture "jobId" (Id Job)
        :> "effects"
        :> Capture "attribute" AttributePath
        :> "log"
        :> "lines"
        :> QueryParam' '[Required] "logId" (Id "log")
        :> QueryParam' '[Optional] "iMin" Int
        :> auth
        :> Get '[JSON] Log,
    cancelEffect ::
      f :- Summary "Cancel the effect. It will cause the Job to have a failed status."
        :> "jobs"
        :> Capture "jobId" (Id Job)
        :> "effects"
        :> Capture "attribute" AttributePath
        :> "cancel"
        :> auth
        :> Post '[JSON] NoContent
  }
  deriving (Generic)
