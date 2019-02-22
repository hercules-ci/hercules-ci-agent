{-# LANGUAGE DataKinds #-}
module Hercules.API.Auth where

import           Servant.API
import           Servant.API.Generic
import           Hercules.API.Servant
import           Hercules.API.Prelude

-- | Endpoints for authentication
data AuthAPI f = AuthAPI {
  initiateGitHubLogin :: f :-
                         "api" :> "auth" :> "github" :>
                         QueryParam' '[Optional, Strict] "redirect" Text :>
                         Get302 '[PlainText, JSON] '[]
  } deriving Generic
