{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API.Organizations
  ( OrganizationsAPI (..),
  )
where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Organizations.CreateOrganization (CreateOrganization)
import Hercules.API.Organizations.Organization (Organization)
import Hercules.API.Organizations.PaymentLink (PaymentLink)
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic

data OrganizationsAPI auth f
  = OrganizationsAPI
      { findOrganizations ::
          f
            :- Summary "Get all organizations user has admin access to"
            :> auth
            :> "api"
            :> "organizations"
            :> Get '[JSON] [Organization],
        createOrganization ::
          f
            :- Summary "Create a new organization"
            :> auth
            :> "api"
            :> "organizations"
            :> ReqBody '[JSON] CreateOrganization
            :> Post '[JSON] Organization,
        connectAccountToOrganization ::
          f
            :- Summary "Connect an account to an organization"
            :> auth
            :> "api"
            :> "organizations"
            :> Capture "organizationId" (Id Organization)
            :> "accounts"
            :> Capture "accountId" (Id Account)
            :> Post '[JSON] NoContent,
        paymentLinkForOrganization ::
          f
            :- Summary "Generate payment link for an organization"
            :> auth
            :> "api"
            :> "organizations"
            :> Capture "organizationId" (Id Organization)
            :> "paymentLink"
            :> Post '[JSON] PaymentLink
      }
  deriving (Generic)
