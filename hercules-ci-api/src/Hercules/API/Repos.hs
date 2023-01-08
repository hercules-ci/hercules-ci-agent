{-# LANGUAGE DataKinds #-}

module Hercules.API.Repos where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude
import Hercules.API.Repos.Repo (Repo)
import Hercules.API.Repos.RepoKey (RepoKey)
import Servant.API
import Servant.API.Generic

data ReposAPI auth f = ReposAPI
  { reposByOwner ::
      f
        :- Summary "Repositories that the account owns or has explicit access to."
          :> "accounts"
          :> Capture' '[Required, Strict] "accountId" (Id Account)
          :> "repos"
          :> auth
          :> Get '[JSON] [Repo],
    parseGitURL ::
      f
        :- Summary "Parse a git remote URL into site, owner and repo. Returns 400 if invalid, 404 if the site can not be determined. Does provide any guarantee that the repository exists."
          :> "parse-git-url"
          :> QueryParam' '[Required, Strict] "gitURL" Text
          :> auth
          :> Get '[JSON] RepoKey
  }
  deriving (Generic)
