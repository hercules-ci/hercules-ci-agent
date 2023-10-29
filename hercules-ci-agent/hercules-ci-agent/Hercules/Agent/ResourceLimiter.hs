-- | Limit concurrent access to a resource. Wrapper around 'TSem`.
module Hercules.Agent.ResourceLimiter where

import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Protolude hiding (atomically, bracket_)
import UnliftIO (MonadUnliftIO, atomically, bracket_)

-- | Limit concurrent access to a resource. Wrapper around 'TSem` semaphore.
newtype ResourceLimiter = ResourceLimiter TSem

-- | Create a semaphore.
newResourceLimiter :: MonadIO m => Integer -> m ResourceLimiter
newResourceLimiter n =
  if n < 1
    then panic "newResourceLimiter: concurrency must be at least 1"
    else atomically $ do
      ResourceLimiter <$> newTSem n

-- | Perform an action while holding a resource.
--
-- To avoid priority inversion and deadlock, use this around small operations, preferably.
withResource :: MonadUnliftIO m => ResourceLimiter -> m a -> m a
withResource (ResourceLimiter sem) = bracket_ (atomically (waitTSem sem)) (atomically (signalTSem sem))
