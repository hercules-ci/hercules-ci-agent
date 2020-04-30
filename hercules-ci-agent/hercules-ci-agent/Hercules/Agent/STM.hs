module Hercules.Agent.STM
  ( module Hercules.Agent.STM,
    module Control.Concurrent.STM,
  )
where

import Control.Concurrent.STM (STM, TVar, readTVar, writeTVar)
import qualified Control.Concurrent.STM as STM
import Protolude hiding (atomically)

atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO

newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . STM.newTVarIO

-- | Drop-in replacement for atomicModifyIORef
modifyTVarIO :: MonadIO m => TVar a -> (a -> (a, b)) -> m b
modifyTVarIO tvar f = atomically $ do
  a0 <- readTVar tvar
  let (a1, b) = f a0
  writeTVar tvar a1
  pure b
