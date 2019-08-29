{-# LANGUAGE DataKinds #-}

module Hercules.API.Auth where

import Hercules.API.Prelude
import Hercules.API.Servant
import Servant.API
import Servant.API.Generic

-- | Endpoints for authentication
data AuthAPI f
  = AuthAPI
      { initiateGitHubLogin
          :: f
               :- "api"
               :> "auth"
               :> "github"
               :> QueryParam' '[Optional, Strict] "redirect" Text
               :> Get302 '[PlainText, JSON] '[]
        }
  deriving (Generic)
