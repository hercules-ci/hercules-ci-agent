{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Hercules.API.Logs
  ( LogsAPI(..)
  )
where

import           Hercules.API.Prelude
import           Servant.API
import           Servant.Auth
import           Servant.API.Generic
import           Data.ByteString                ( ByteString )

data LogsAPI logJWT f = LogsAPI
  { writeLog :: f :-
      Summary "Write to a log" :>
      Description "Writes an entire log in a single request. Provide a log-specific token for authentication." :>
      "log" :>
      Auth '[JWT] logJWT :>
      ReqBody '[OctetStream] ByteString :>
      Post '[JSON] NoContent
  } deriving Generic
