{-# LANGUAGE DataKinds #-}

module Hercules.API.Repos where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude
import Hercules.API.Repos.Repo
import Servant.API
import Servant.API.Generic

data ReposAPI auth f
  = ReposAPI
      { reposByOwner
          :: f
               :- Summary "Repositories that the account owns or has explicit access to."
               :> "accounts"
               :> Capture' '[Required, Strict] "accountId" (Id Account)
               :> "repos"
               :> auth
               :> Get '[JSON] [Repo]
        }
  deriving (Generic)
