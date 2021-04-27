{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.API.State where

import Data.ByteString (ByteString)
import Data.Swagger (NamedSchema (NamedSchema), binarySchema)
import Data.Swagger.Schema (ToSchema (..))
import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.SourceHostingSite.SourceHostingSite (SourceHostingSite)
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

data ProjectStateResourceGroup auth f = ProjectStateResourceGroup
  { putStateData ::
      f :- Summary "Upload a state file"
        :> "state"
        :> Capture' '[Required, Strict] "stateName" Text
        :> "data"
        :> StreamBody NoFraming OctetStream (SourceIO RawBytes)
        :> auth
        :> Put '[JSON] NoContent,
    getStates ::
      f :- Summary "List all state files"
        :> "states"
        :> auth
        :> Get '[JSON] ProjectState,
    getStateData ::
      f :- Summary "Download a state file"
        :> "state"
        :> Capture' '[Required, Strict] "stateName" Text
        :> "data"
        :> QueryParam' '[Optional, Strict] "version" Int
        :> auth
        :> StreamGet NoFraming OctetStream (Headers '[ContentLength, ContentDisposition] (SourceIO RawBytes))
  }
  deriving (Generic)

data StateAPI auth f = StateAPI
  { byProjectId ::
      f
        :- Substitute
             ( "projects"
                 :> Capture' '[Required, Strict] "projectId" (Id Project)
                 :> Placeholder
             )
             (ToServantApi (ProjectStateResourceGroup auth)),
    byProjectName ::
      f
        :- Substitute
             ( "site"
                 :> Capture' '[Required, Strict] "site" (Name SourceHostingSite)
                 :> "account"
                 :> Capture' '[Required, Strict] "account" (Name Account)
                 :> "project"
                 :> Capture' '[Required, Strict] "project" (Name Project)
                 :> Placeholder
             )
             (ToServantApi (ProjectStateResourceGroup auth))
  }
  deriving (Generic)
