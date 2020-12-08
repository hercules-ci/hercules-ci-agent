{-# LANGUAGE DataKinds #-}

module Hercules.API.Accounts where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Accounts.AccountSettings (AccountSettings)
import Hercules.API.Accounts.AccountSettingsPatch (AccountSettingsPatch)
import Hercules.API.Accounts.CLIAuthorizationRequest (CLIAuthorizationRequest)
import Hercules.API.Accounts.CLIAuthorizationRequestCreate (CLIAuthorizationRequestCreate)
import Hercules.API.Accounts.CLIAuthorizationRequestCreateResponse (CLIAuthorizationRequestCreateResponse)
import Hercules.API.Accounts.CLIAuthorizationRequestStatus (CLIAuthorizationRequestStatus)
import Hercules.API.Prelude
import Hercules.API.SourceHostingSite.SourceHostingSite
  ( SourceHostingSite,
  )
import Servant.API
import Servant.API.Generic
import Servant.Auth ()

data AccountsAPI auth f = AccountsAPI
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
    postCLIAuthorizationRequest ::
      f :- Summary "Create a request to authorize the CLI."
        :> "auth"
        :> "cli"
        :> "authorization"
        :> "request"
        :> ReqBody '[JSON] CLIAuthorizationRequestCreate
        :> Post '[JSON] CLIAuthorizationRequestCreateResponse,
    getCLIAuthorizationRequestStatus ::
      f :- Summary "Check the request status"
        :> "auth"
        :> "cli"
        :> "authorization"
        :> "request"
        :> "status"
        :> Capture "temporaryToken" Text
        :> Get '[JSON] CLIAuthorizationRequestStatus,
    getCLIAuthorizationRequest ::
      f :- Summary "Retrieve the request"
        :> "auth"
        :> "cli"
        :> "authorization"
        :> "request"
        :> Capture "browserToken" Text
        :> auth
        :> Get '[JSON] CLIAuthorizationRequest,
    confirmCLIAuthorizationRequest ::
      f :- Summary "Retrieve the request"
        :> "auth"
        :> "cli"
        :> "authorization"
        :> "request"
        :> Capture "browserToken" Text
        :> "confirm"
        :> auth
        :> Post '[JSON] NoContent,
    postDisableAllProjects ::
      f :- Summary "Disable all projects in the account."
        :> "accounts"
        :> Capture "accountId" (Id Account)
        :> "disable-all-projects"
        :> auth
        :> Post '[JSON] Int
  }
  deriving (Generic)
