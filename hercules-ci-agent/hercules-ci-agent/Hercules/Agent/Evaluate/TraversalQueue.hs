-- | Couldn't think of a better name.
--
--  * it dispatches via 'Chan'
--  * keeps track of how many items are being processed (size)
--  * and does things only once.
module Hercules.Agent.Evaluate.TraversalQueue
  ( with,
    work,
    waitUntilDone,
    enqueue,
    close,
    Queue ()
    )
where

import Control.Concurrent.Chan.Lifted
import Control.Concurrent.STM
import Control.Exception.Lifted (bracket)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.IORef.Lifted
import qualified Data.Set as Set
import Protolude hiding
  ( bracket,
    newChan,
    newQSem,
    newQSemN,
    readChan,
    writeChan
    )

data Queue a
  = Queue
      { chan :: Chan (Maybe a),
        visitedSet :: IORef (Set a),
        size :: TVar Int
        -- ^ Increased on enqueue, decreased when processed
        }

with :: MonadBaseControl IO m => (Queue a -> m ()) -> m ()
with = Control.Exception.Lifted.bracket new close

new :: MonadBase IO m => m (Queue a)
new = Queue <$> newChan <*> newIORef Set.empty <*> liftBase (newTVarIO 0)

close :: MonadBase IO m => Queue a -> m ()
close env = writeChan (chan env) Nothing

enqueue :: MonadBase IO m => Queue a -> a -> m ()
enqueue env msg = do
  liftBase $ atomically $ modifyTVar (size env) (+ 1)
  -- Not transactional but shouldn't hurt much since we fail the whole thing
  -- in erlang style anyway using the async library.
  writeChan (chan env) (Just msg)

waitUntilDone :: MonadBase IO m => Queue a -> m ()
waitUntilDone env = liftBase $ atomically $ do
  n <- readTVar (size env)
  check (n == 0)

readJust_
  :: (MonadBase IO m, MonadIO m)
  => Chan (Maybe a)
  -> (a -> m ())
  -> m ()
readJust_ ch f = do
  mmsg <- readChan ch
  case mmsg of
    Nothing -> writeChan ch Nothing
    Just msg -> f msg

work
  :: (MonadBase IO m, MonadIO m, Ord a)
  => Queue a
  -> ((a -> m ()) -> a -> m ())
  -> m ()
work env f = work' env $ \snapshot ->
  let enqueue' item = unless (item `Set.member` snapshot) $ enqueue env item
   in f enqueue'

work'
  :: (MonadBase IO m, MonadIO m, Ord a)
  => Queue a
  -> (Set a -> a -> m ())
  -> m ()
work' env f = readJust_ (chan env) $ \msg -> do
  snapshotWhenInserted <-
    atomicModifyIORef (visitedSet env) $ \st ->
      if Set.member msg st
        then (st, Nothing)
        else let st' = Set.insert msg st in (st', Just st')
  forM_ snapshotWhenInserted $ \snapshot -> f snapshot msg
  liftBase $ atomically $ modifyTVar (size env) (subtract 1)
  work' env f
