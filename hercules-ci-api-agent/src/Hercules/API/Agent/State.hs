{-# LANGUAGE DataKinds #-}

module Hercules.API.Agent.State where

import Data.ByteString (ByteString)
import Hercules.API.Prelude
import Servant.API

type ContentLength = Header "Content-Length" Integer

data StateAPI auth f = StateAPI
  { getState ::
      f
        :- "current-task"
        :> "state"
        :> Capture' '[Required] "name" Text
        :> "data"
        :> auth
        :> StreamGet NoFraming OctetStream (Headers '[ContentLength] (SourceIO ByteString)),
    putState ::
      f
        :- "current-task"
        :> "state"
        :> Capture' '[Required] "name" Text
        :> "data"
        :> StreamBody NoFraming OctetStream (SourceIO ByteString)
        :> ContentLength
        :> auth
        :> Put '[JSON] NoContent
  }
  deriving (Generic)
