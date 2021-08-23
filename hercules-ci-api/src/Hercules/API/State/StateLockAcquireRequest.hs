{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.State.StateLockAcquireRequest where

import Hercules.API.Prelude

data StateLockAcquireRequest = StateLockAcquireRequest
  { -- | A description of the activity that the lock is for. This may appear in
    -- logs when other clients are blocked.
    description :: Text,
    -- | @True@ to request an exclusive lock. Non-exclusive locks are only mutually exclusive with exclusive locks.
    exclusive :: Bool,
    -- | For recursive locking. Set this to the value of environment variable
    -- HERCULES_CI_LOCK_LEASE_ID when present.
    parent :: Maybe (Id "StateLockLease"),
    -- | Generate a random key to make sure that a retry can be successful.
    idempotencyKey :: Maybe (Id "IdempotencyKey")
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
