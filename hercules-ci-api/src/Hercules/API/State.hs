{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.API.State where

import Data.ByteString (ByteString)
import Data.Swagger (NamedSchema (NamedSchema), binarySchema)
import Data.Swagger.Schema (ToSchema (..))
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.State.ProjectState (ProjectState)
import Servant.API
import Servant.API.Generic

-- | A newtype wrapper for servant-swagger
newtype RawBytes = RawBytes {fromRawBytes :: ByteString}
  deriving newtype (MimeUnrender OctetStream, MimeRender OctetStream)

instance ToSchema RawBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "RawBytes") binarySchema

type ContentLength = Header "Content-Length" Integer

type ContentDisposition = Header "Content-Disposition" Text

data StateAPI auth f = StateAPI
  { putProjectStateData ::
      f
        :- Summary "Upload a state file"
        :> "projects"
        :> Capture' '[Required, Strict] "projectId" (Id Project)
        :> "state"
        :> Capture' '[Required, Strict] "stateName" Text
        :> "data"
        :> StreamBody NoFraming OctetStream (SourceIO RawBytes)
        :> auth
        :> Put '[JSON] NoContent,
    getProjectStates ::
      f
        :- Summary "List all state files"
        :> "projects"
        :> Capture' '[Required, Strict] "projectId" (Id Project)
        :> "states"
        :> auth
        :> Get '[JSON] ProjectState,
    getProjectStateData ::
      f
        :- Summary "Download a state file"
        :> "projects"
        :> Capture' '[Required, Strict] "projectId" (Id Project)
        :> "state"
        :> Capture' '[Required, Strict] "stateName" Text
        :> "data"
        :> auth
        :> StreamGet NoFraming OctetStream (Headers '[ContentLength, ContentDisposition] (SourceIO RawBytes))
  }
  deriving (Generic)
