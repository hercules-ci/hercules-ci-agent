{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API.Logs
  ( LogsAPI (..),
  )
where

import Data.ByteString (ByteString)
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic
import Servant.Auth

data LogsAPI logJWT f
  = LogsAPI
      { writeLog ::
          f
            :- Summary "Write to a log"
            :> Description "Writes an entire log in a single request. Provide a log-specific token for authentication."
            :> "log"
            :> Auth '[JWT] logJWT
            :> ReqBody '[OctetStream] ByteString
            :> Post '[JSON] NoContent
      }
  deriving (Generic)
