{-# LANGUAGE DataKinds #-}

module Hercules.API.Health where

import Servant.API
import Servant.API.Generic

data HealthAPI auth f
  = HealthAPI
      { db
          :: f
               :- Summary "Health check for the database"
               :> "health"
               :> "db"
               :> Get '[JSON] NoContent,
        queue
          :: f
               :- Summary "Health check for the queue"
               :> "health"
               :> "queue"
               :> Get '[JSON] NoContent,
        github
          :: f
               :- Summary "Health check for the github"
               :> "health"
               :> "github"
               :> Get '[JSON] NoContent
        }
  deriving (Generic)
