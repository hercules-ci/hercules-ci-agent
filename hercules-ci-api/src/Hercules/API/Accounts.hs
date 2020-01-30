{-# LANGUAGE DataKinds #-}

module Hercules.API.Accounts where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Accounts.AccountSettings (AccountSettings)
import Hercules.API.Accounts.AccountSettingsPatch (AccountSettingsPatch)
import Hercules.API.Prelude
import Hercules.API.SourceHostingSite.SourceHostingSite
  ( SourceHostingSite,
  )
import Servant.API
import Servant.API.Generic
import Servant.Auth ()

data AccountsAPI auth f
  = AccountsAPI
      { myAccount ::
          f
            :- Summary "The account of the authenticated user."
            :> "accounts"
            :> "me"
            :> auth
            :> Get '[JSON] Account,
        findAccounts ::
          f
            :- Summary "Accounts that the authenticated user owns, admins or collaborates with."
            :> "accounts"
            :> QueryParam "site" (Name SourceHostingSite)
            :> QueryParam "name" (Name Account)
            :> auth
            :> Get '[JSON] [Account],
        getAccountSettings ::
          f :- Summary "Retrieve the account settings"
            :> "accounts"
            :> Capture "accountId" (Id Account)
            :> "settings"
            :> auth
            :> Get '[JSON] AccountSettings,
        patchAccountSettings ::
          f :- Summary "Update the account settings"
            :> "accounts"
            :> Capture "accountId" (Id Account)
            :> "settings"
            :> ReqBody '[JSON] AccountSettingsPatch
            :> auth
            :> Patch '[JSON] AccountSettings,
        postDisableAllProjects ::
          f :- Summary "Disable all projects in the account."
            :> "accounts"
            :> Capture "accountId" (Id Account)
            :> "disable-all-projects"
            :> auth
            :> Post '[JSON] Int
      }
  deriving (Generic)
