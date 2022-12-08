{-# LANGUAGE DataKinds #-}

module Hercules.API.Auth where

import Hercules.API.Prelude
import Hercules.API.Servant.Status
import Servant.API
import Web.Cookie (SetCookie)

-- | Endpoints for authentication
data AuthAPI auth f = AuthAPI
  { initiateGitHubLogin ::
      f :- "api"
        :> "auth"
        :> "github"
        :> QueryParam' '[Optional, Strict] "redirect" Text
        :> Get302 '[PlainText, JSON] '[],
    signOut ::
      f :- "api"
        :> "auth"
        :> "sign-out"
        :> auth
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  }
  deriving (Generic)
