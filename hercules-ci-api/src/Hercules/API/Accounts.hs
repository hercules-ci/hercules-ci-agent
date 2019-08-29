{-# LANGUAGE DataKinds #-}

module Hercules.API.Accounts where

import Hercules.API.Accounts.Account
import Hercules.API.Prelude
import Hercules.API.SourceHostingSite.SourceHostingSite
  ( SourceHostingSite
    )
import Servant.API
import Servant.API.Generic
import Servant.Auth ()

data AccountsAPI auth f
  = AccountsAPI
      { myAccount
          :: f
               :- Summary "The account of the authenticated user."
               :> "accounts"
               :> "me"
               :> auth
               :> Get '[JSON] Account,
        findAccounts
          :: f
               :- Summary "Accounts that the authenticated user owns, admins or collaborates with."
               :> "accounts"
               :> QueryParam "site" (Name SourceHostingSite)
               :> QueryParam "name" (Name Account)
               :> auth
               :> Get '[JSON] [Account]
        }
  deriving (Generic)
