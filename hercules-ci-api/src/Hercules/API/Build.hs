{-# LANGUAGE DataKinds #-}

module Hercules.API.Build where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Build.DerivationInfo (DerivationInfo)
import Hercules.API.Build.Log (Log)
import Hercules.API.Prelude
import Hercules.API.Projects.Job (Job)
import Servant.API
import Servant.API.Generic

data BuildAPI auth f = BuildAPI
  { restartDerivation ::
      f
        :- Summary "Restart a derivation"
          :> "accounts"
          :> Capture "accountId" (Id Account)
          :> "derivations"
          :> Capture "derivationPath" Text
          :> "retry"
          :> auth
          :> Post '[PlainText, JSON] NoContent,
    cancelDerivation ::
      f
        :- Summary "Cancel a derivation"
          :> Description "If running, the build or push process will be killed. It will not be restarted, unless a rebuild is requested, or when output contents are required during evaluation (import from derivation)."
          :> "accounts"
          :> Capture "accountId" (Id Account)
          :> "derivations"
          :> Capture "derivationPath" Text
          :> "cancel"
          :> auth
          :> Post '[PlainText, JSON] NoContent,
    readDerivationLogText ::
      f
        :- Summary "Read a derivation build log"
          :> Description "This interface may change."
          :> "accounts"
          :> Capture "accountId" (Id Account)
          :> "derivations"
          :> Capture "derivationPath" Text
          :> "log"
          :> QueryParam "logId" (Id "log")
          :> auth
          :> Get '[PlainText, JSON] Text, -- NB: We use JSON only to be able to generate elm api
          -- FIXME: bytes?
    getLog ::
      f
        :- Summary "Read all recorded log entries"
          :> "accounts"
          :> Capture "accountId" (Id Account)
          :> "derivations"
          :> Capture "derivationPath" Text
          :> "log"
          :> "lines"
          :> QueryParam' '[Required] "logId" (Id "log")
          :> QueryParam' '[Optional] "iMin" Int
          :> auth
          :> Get '[JSON] Log,
    getDerivationInfo ::
      f
        :- Summary "Get information about a derivation."
          :> Description "Optionally, a job id can be specified to provide context."
          :> "accounts"
          :> Capture "accountId" (Id Account)
          :> "derivations"
          :> Capture "derivationPath" Text
          :> QueryParam' '[Optional, Strict] "via-job" (Id Job)
          :> auth
          :> Get '[JSON] DerivationInfo
  }
  deriving (Generic)
