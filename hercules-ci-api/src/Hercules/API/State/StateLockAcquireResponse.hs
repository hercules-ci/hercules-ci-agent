{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.State.StateLockAcquireResponse where

import Hercules.API.Prelude
import Hercules.API.State.StateLockLease (StateLockLease)

data StateLockAcquireResponse
  = Acquired StateLockAcquiredResponse
  | Blocked StateLockBlockedResponse
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data StateLockAcquiredResponse = StateLockAcquiredResponse
  { leaseId :: Id "StateLockLease",
    expirationTime :: UTCTime
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data StateLockBlockedResponse = LockBlockedResponse
  { blockedByLeases :: [StateLockLease]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
