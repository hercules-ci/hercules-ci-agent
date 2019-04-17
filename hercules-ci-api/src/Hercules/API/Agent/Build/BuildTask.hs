{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Build.BuildTask where

import           Prelude
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )
import           Hercules.API.Id
import           Hercules.API.Task              ( Task )

data BuildTask = BuildTask
  { id :: Id (Task BuildTask)
  , derivationPath :: Text
  , logToken :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
