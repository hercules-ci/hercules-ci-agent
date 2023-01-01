{-# LANGUAGE DataKinds #-}

module Hercules.API.Auth where

import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude
import Hercules.API.Servant.Status
import Servant.API
import Web.Cookie (SetCookie)

type OAuthReturn stage view =
  "api"
    :> "auth"
    :> "forge"
    :> Capture "forge" (Id Forge)
    :> "return"
    :> QueryParam' '[stage, Strict] "code" Text
    :> QueryParam' '[Optional, Strict] "redirect" Text
    :> view

-- | Endpoints for authentication
data AuthAPI auth f = AuthAPI
  { initiateGitHubLogin ::
      f :- "api"
        :> "auth"
        :> "github"
        :> QueryParam' '[Optional, Strict] "redirect" Text
        :> Get302 '[PlainText, JSON] '[],
    signOut ::
      f :- Summary "Terminate the session and help clear the cookies."
        :> "api"
        :> "auth"
        :> "sign-out"
        :> auth
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent),
    start ::
      f :- Summary "Initiate an OAuth login, redirecting the client to a URL at the OAuth provider."
        :> "api"
        :> "auth"
        :> Capture "type" Text
        :> "start"
        :> Capture "forgeId" (Id Forge)
        :> QueryParam' '[Optional, Strict] "redirect" Text
        :> auth
        :> Get302 '[PlainText, JSON] '[],
    return ::
      f
        :- OAuthReturn
             Required
             ( auth
                 :> Get302
                      '[PlainText, JSON]
                      '[ Header "Set-Cookie" SetCookie,
                         Header "Set-Cookie" SetCookie
                       ]
             )
  }
  deriving (Generic)

data AuthRoutes view f = AuthRoutes
  { authRouteReturn :: f :- OAuthReturn Optional view
  }
  deriving (Generic)
