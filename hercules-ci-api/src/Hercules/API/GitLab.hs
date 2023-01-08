{-# LANGUAGE DataKinds #-}

module Hercules.API.GitLab where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.GitLab.CreateInstallationBuilderRequest (CreateInstallationBuilderRequest)
import Hercules.API.GitLab.InstallationBuilder (InstallationBuilder, InstallationBuilders)
import Hercules.API.GitLab.PatchInstallationBuilder (PatchInstallationBuilder)
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic

data GitLabAPI auth f = GitLabAPI
  { createInstallationBuilder ::
      f
        :- "gitlab"
          :> "installation"
          :> ReqBody '[JSON] CreateInstallationBuilderRequest
          :> auth
          :> Post '[JSON] InstallationBuilder,
    getInstallationBuilders ::
      f
        :- "gitlab"
          :> "installations"
          :> auth
          :> Get '[JSON] InstallationBuilders,
    getInstallationBuilder ::
      f
        :- "gitlab"
          :> "installation"
          :> Capture "installationId" (Id InstallationBuilder)
          :> auth
          :> Get '[JSON] InstallationBuilder,
    patchInstallationBuilder ::
      f
        :- "gitlab"
          :> "installation"
          :> Capture "installationId" (Id InstallationBuilder)
          :> auth
          :> ReqBody '[JSON] PatchInstallationBuilder
          :> Patch '[JSON] InstallationBuilder,
    deleteInstallationBuilder ::
      f
        :- "gitlab"
          :> "installation"
          :> Capture "installationId" (Id InstallationBuilder)
          :> auth
          :> Delete '[JSON] NoContent,
    installAccount ::
      f
        :- "accounts"
          :> Capture "accountId" (Id Account)
          :> "gitlab"
          :> "install"
          :> auth
          :> Post '[JSON] NoContent,
    deinstallAccount ::
      f
        :- "accounts"
          :> Capture "accountId" (Id Account)
          :> "gitlab"
          :> "deinstall"
          :> auth
          :> Post '[JSON] NoContent
  }
  deriving (Generic)
