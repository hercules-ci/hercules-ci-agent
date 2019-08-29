{-# LANGUAGE DataKinds #-}

module Hercules.API.Build where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic

data BuildAPI auth f
  = BuildAPI
      { restartDerivation
          :: f
               :- Summary "Restart a failed derivation"
               :> "accounts"
               :> Capture "accountId" (Id Account)
               :> "derivations"
               :> Capture "derivationPath" Text
               :> "retry"
               :> auth
               :> Post '[PlainText, JSON] NoContent,
        readDerivationLogText
          :: f
               :- Summary "Read a derivation build log"
               :> Description "This interface may change."
               :> "accounts"
               :> Capture "accountId" (Id Account)
               :> "derivations"
               :> Capture "derivationPath" Text
               :> "log"
               :> auth
               :> Get '[PlainText, JSON] Text -- NB: We use JSON only to be able to generate elm api
                 -- FIXME: bytes?
        }
  deriving (Generic)
