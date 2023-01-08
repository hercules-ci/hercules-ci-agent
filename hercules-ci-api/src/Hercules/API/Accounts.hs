{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Hercules.API.Accounts where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Accounts.AccountInstallationStatus (AccountInstallationStatus)
import Hercules.API.Accounts.AccountSettings (AccountSettings)
import Hercules.API.Accounts.AccountSettingsPatch (AccountSettingsPatch)
import Hercules.API.Accounts.CLIAuthorizationRequest (CLIAuthorizationRequest)
import Hercules.API.Accounts.CLIAuthorizationRequestCreate (CLIAuthorizationRequestCreate)
import Hercules.API.Accounts.CLIAuthorizationRequestCreateResponse (CLIAuthorizationRequestCreateResponse)
import Hercules.API.Accounts.CLIAuthorizationRequestStatus (CLIAuthorizationRequestStatus)
import Hercules.API.Accounts.CLITokensResponse (CLITokensResponse)
import Hercules.API.Accounts.NotificationSettings (NotificationSettings)
import Hercules.API.Accounts.NotificationSettingsPatch (NotificationSettingsPatch)
import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude hiding (id)
import Servant.API
import Servant.API.Generic
import Servant.Auth ()

data AccountResourceGroup auth f = AccountResourceGroup
  { get ::
      f
        :- Summary ("Get the account.")
          :> auth
          :> Get '[JSON] Account,
    getSettings ::
      f
        :- Summary ("Get the account settings.")
          :> "settings"
          :> auth
          :> Get '[JSON] AccountSettings,
    patchSettings ::
      f
        :- Summary ("Update the account settings.")
          :> "settings"
          :> ReqBody '[JSON] AccountSettingsPatch
          :> auth
          :> Patch '[JSON] AccountSettings,
    postDisableAllProjects ::
      f
        :- Summary ("Disable all projects in the account.")
          :> "disable-all-projects"
          :> auth
          :> Post '[JSON] Int
  }
  deriving (Generic)

accountById' id client = client `enterApiE` \api -> accountById api id

accountByAuth' client = client `enterApiE` accountByAuth

data AccountsAPI auth f = AccountsAPI
  { accountByAuth ::
      f
        :- Substitute ("accounts" :> "me" :> Placeholder) (ToServantApi (AccountResourceGroup auth)),
    accountByAuthGetNotificationSettings ::
      f
        :- Summary "Retrieve notification settings"
          :> "accounts"
          :> "me"
          :> "settings"
          :> "notifications"
          :> auth
          :> Get '[JSON] NotificationSettings,
    accountByAuthPatchNotificationSettings ::
      f
        :- Summary "Update notification settings"
          :> "accounts"
          :> "me"
          :> "settings"
          :> "notifications"
          :> ReqBody '[JSON] NotificationSettingsPatch
          :> auth
          :> Patch '[JSON] NotificationSettings,
    accountById ::
      f
        :- Substitute
             ("accounts" :> Capture "accountId" (Id Account) :> Placeholder)
             (ToServantApi (AccountResourceGroup auth)),
    accountByName ::
      f
        :- Substitute
             ( "site"
                 :> Capture "site" (Name Forge)
                 :> "account"
                 :> Capture "account" (Name Account)
                 :> Placeholder
             )
             (ToServantApi (AccountResourceGroup auth)),
    findAccounts ::
      f
        :- Summary "Accounts that the authenticated user owns, admins or collaborates with."
          :> "accounts"
          :> QueryParam "site" (Name Forge)
          :> QueryParam "name" (Name Account)
          :> auth
          :> Get '[JSON] [Account],
    postCLIAuthorizationRequest ::
      f
        :- Summary "Create a request to authorize the CLI."
          :> "auth"
          :> "cli"
          :> "authorization"
          :> "request"
          :> ReqBody '[JSON] CLIAuthorizationRequestCreate
          :> Post '[JSON] CLIAuthorizationRequestCreateResponse,
    getCLIAuthorizationRequestStatus ::
      f
        :- Summary "Check the request status"
          :> "auth"
          :> "cli"
          :> "authorization"
          :> "request"
          :> "status"
          :> Capture "temporaryToken" Text
          :> Get '[JSON] CLIAuthorizationRequestStatus,
    getCLIAuthorizationRequest ::
      f
        :- Summary "Retrieve the request"
          :> "auth"
          :> "cli"
          :> "authorization"
          :> "request"
          :> Capture "browserToken" Text
          :> auth
          :> Get '[JSON] CLIAuthorizationRequest,
    confirmCLIAuthorizationRequest ::
      f
        :- Summary "Retrieve the request"
          :> "auth"
          :> "cli"
          :> "authorization"
          :> "request"
          :> Capture "browserToken" Text
          :> "confirm"
          :> auth
          :> Post '[JSON] NoContent,
    getCLITokens ::
      f
        :- Summary "List the CLI tokens associated with the current account."
          :> "auth"
          :> "cli"
          :> "tokens"
          :> auth
          :> Get '[JSON] CLITokensResponse,
    revokeCLIToken ::
      f
        :- Summary "Permanently disallow the use of a CLI token."
          :> "auth"
          :> "cli"
          :> "tokens"
          :> Capture "cliTokenId" (Id "CLIToken")
          :> "revoke"
          :> auth
          :> Post '[JSON] NoContent,
    installationStatus ::
      f
        :- Summary "Retrieve installation status after redirect from external source site settings."
          :> "sites"
          :> Capture "forgeId" (Id Forge)
          :> "installation"
          :> Capture "installationId" Int
          :> "status"
          :> auth
          :> Get '[JSON] AccountInstallationStatus
  }
  deriving (Generic)
