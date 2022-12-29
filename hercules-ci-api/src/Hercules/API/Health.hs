{-# LANGUAGE DataKinds #-}

module Hercules.API.Health where

import Hercules.API.Prelude
import Servant.API

data HealthAPI auth f = HealthAPI
  { db ::
      f
        :- Summary "Health check for the database"
          :> "health"
          :> "db"
          :> Get '[JSON] NoContent,
    queue ::
      f
        :- Summary "Health check for the queue"
          :> "health"
          :> "queue"
          :> Get '[JSON] NoContent,
    github ::
      f
        :- Summary "Health check for the github"
          :> "health"
          :> "github"
          :> Get '[JSON] NoContent
  }
  deriving (Generic)
