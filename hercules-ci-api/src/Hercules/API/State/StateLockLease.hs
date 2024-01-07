{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.State.StateLockLease where

import Data.OpenApi qualified as O3
import Hercules.API.Accounts.SimpleAccount (SimpleAccount)
import Hercules.API.Prelude
import Hercules.API.Projects.SimpleJob (SimpleJob)

data StateLockLease = StateLockLease
  { id :: Id "StateLockLease",
    -- | When the lease was granted.
    startTime :: UTCTime,
    -- | Time of expiry. The lease can be terminated or extended by the actor.
    expirationTime :: UTCTime,
    -- | In case the lock was acquired by a user.
    user :: Maybe SimpleAccount,
    -- | In case the lock was acquired by an effect.
    job :: Maybe SimpleJob,
    -- | User-provided text describing the reason to lock. May be empty.
    description :: Text,
    -- | A pre-existing lock lease. This allows a lease to be granted when the
    -- actor knows it already has a lock lease.
    parent :: Maybe (Id "StateLockLease"),
    -- | Whether the lock is exclusive.
    exclusive :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
