{-# LANGUAGE DataKinds #-}

module Hercules.API.ClientInfo where

import Hercules.API.ClientInfo.ClientInfo (ClientInfo)
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic

data ClientInfoAPI auth f = ClientInfoAPI
  { getClientInfo ::
      f
        :- "client"
          :> "info"
          :> auth
          :> Get '[JSON] ClientInfo
  }
  deriving (Generic)
