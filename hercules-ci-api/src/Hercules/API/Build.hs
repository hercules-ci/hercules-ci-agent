{-# LANGUAGE DataKinds #-}
module Hercules.API.Build where

import           Servant.API
import           Servant.API.Generic
import           Hercules.API.Prelude
import           Hercules.API.Accounts.Account  ( Account )

data BuildAPI auth f = BuildAPI
  { restartDerivation :: f :-
        Summary "Restart a failed derivation" :>
        "accounts" :>
        Capture "accountId" (Id Account) :>
        "derivations" :>
        Capture "derivationPath" Text :>
        "retry" :>
        auth :>
        Post '[PlainText, JSON] NoContent

  , readDerivationLogText :: f :-
       Summary "Read a derivation build log" :>
       Description "This interface may change." :>
       "accounts" :>
       Capture "accountId" (Id Account) :>
       "derivations" :>
       Capture "derivationPath" Text :>
       "log" :>
       auth :>
       -- NB: We use JSON only to be able to generate elm api
       Get '[PlainText, JSON] Text -- FIXME: bytes?
  } deriving Generic
