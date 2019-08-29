{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Producer where

import Control.Applicative
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async hiding (cancel)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.State
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Katip
import UnliftIO.Exception
import Prelude

-- | A thread producing zero or more payloads and a final value.
-- Handles exception propagation.
data Producer p r
  = Producer
      { producerQueueRead :: STM (Msg p r),
        producerThread :: ThreadId
        }
  deriving (Functor)

data ProducerCancelled = ProducerCancelled
  deriving (Show, Exception, Typeable)

data Msg p r
  = Payload p -- ^ One of possibly many payloads from the producer
  | Exception SomeException -- ^ The producer stopped due to an exception
  | Close r -- ^ The producer was done and produced a final value
  deriving (Functor)

-- | @forkProducer f@ produces a computation that forks a thread for @f@, which
-- receives a function for returning payloads @p@.
--
-- @f@ may produce a final result value @r@ when it is done.
forkProducer :: forall m p r. (MonadIO m, MonadUnliftIO m) => ((p -> m ()) -> m r) -> m (Producer p r)
forkProducer f = do
  q <- liftIO newTQueueIO
  let write :: MonadIO  m' => Msg p r ->  m' ()
      write = liftIO . atomically . writeTQueue q
  f' <- toIO (f (write . Payload))
  t <- liftIO $ forkFinally f' (write . toResult)
  pure $ Producer {producerQueueRead = readTQueue q, producerThread = t}
  where
    toResult (Left e) = Exception e
    toResult (Right r) = Close r

-- | Throws 'ProducerCancelled' as an async exception to the producer thread.
-- Blocks until exception is raised. See 'throwTo'.
cancel :: MonadIO m => Producer p r -> m ()
cancel p = liftIO $ throwTo (producerThread p) ProducerCancelled

-- | Perform an computation while @withProducer@ takes care of forking and cleaning up.
--
-- @withProducer (\write -> write "a" >> write "b") $ \producer -> consume producer@
withProducer
  :: (MonadIO m, MonadUnliftIO m)
  => ((p -> m ()) -> m r)
  -> (Producer p r -> m a)
  -> m a
withProducer f = bracket (forkProducer f) cancel

listen
  :: MonadIO m
  => Producer p r
  -> (p -> m a)
  -> (r -> m a)
  -> STM (m a)
listen p fPayload fResult =
  fmap f (producerQueueRead p)
  where
    f (Payload payload) = fPayload payload
    f (Exception e) = throwIO e
    f (Close r) = fResult r

joinSTM :: MonadIO m => STM (m a) -> m a
joinSTM = join . liftIO . atomically

data Syncing a = Syncable a | Syncer (Maybe SomeException -> STM ())

-- | Sends sync notifications after the whole computation succeeds (or fails)
-- Note: not exception safe in the presence of pure exceptions.
withSync
  :: (MonadIO m, MonadUnliftIO m, Traversable t)
  => t (Syncing a)
  -> (t (Maybe a) -> m b)
  -> m b
withSync t f = do
  let (t', syncs) =
        runState
          ( for t $ \case
              Syncable a -> pure (Just a)
              Syncer s -> Nothing <$ modify (*> s)
            )
          (\_ -> pure ())
  b <- f t' `withException` (liftIO . atomically . syncs . Just)
  liftIO $ atomically $ syncs Nothing
  pure b

--  where trav =
--  deriving (Functor)
--instance Applicative Syncing where
--  pure = Synced Nothing
--  Synced sf f <*> Synced af a = Synced (sf <> af) (f a)

-- Potential improvements:
--  - Performance: Get rid of the producer thread by doing the batching in STM.
--                 (pinging thread still required)
--  - Performance: Multiple elements as input
--  - Idle footprint: The pinger can be made to wait for the queue to be non-empty before starting the delay.
--     - Add a tryPeek function to Producer
--     - Make sure it is not woken up after the queue has become non-empty
--     - Alternatively, maybe use stm-delay (which uses GHC.Event for efficiency)
--       https://hackage.haskell.org/package/stm-delay-0.1.1.1/docs/Control-Concurrent-STM-Delay.html
withBoundedDelayBatchProducer
  :: (MonadIO m, MonadUnliftIO m, KatipContext m)
  => Int -- ^ Max time before flushing in microseconds
  -> Int -- ^ Max number of items in batch
  -> Producer p r
  -> (Producer [p] r -> m a)
  -> m a
withBoundedDelayBatchProducer maxDelay maxItems sourceP f = do
  UnliftIO {unliftIO = unlift} <- askUnliftIO
  flushes <- liftIO $ newTQueueIO
  let producer writeBatch =
        let beginReading = readItems (max 1 maxItems) []
            doPerformBatch [] = pure ()
            doPerformBatch buf = writeBatch (reverse buf)
            readItems 0 buf = do
              --logLocM DebugS "batch on full"
              doPerformBatch buf
              beginReading
            readItems bufferRemaining buf =
              joinSTM
                ( onQueueRead <$> producerQueueRead sourceP
                    <|> onFlush
                    <$ readTQueue flushes
                  )
              where
                onQueueRead (Payload a) =
                  readItems (bufferRemaining - 1) (a : buf)
                onQueueRead (Close r) = do
                  --logLocM DebugS $ "batch on close: " <> logStr (show (length buf))
                  doPerformBatch buf
                  pure r
                onQueueRead (Exception e) = do
                  --logLocM DebugS $ "batch on exception: " <> logStr (show (length buf))
                  doPerformBatch buf
                  liftIO $ throwIO e
                onFlush = do
                  --logLocM DebugS $ "batch on flush: " <> logStr (show (length buf))
                  doPerformBatch buf
                  beginReading
         in beginReading
  liftIO
    $ withAsync
        ( forever $ do
            threadDelay maxDelay
            atomically $ writeTQueue flushes ()
          )
    $ \_flusher -> unlift $ withProducer producer f

syncer :: MonadIO m => (Syncing a -> m ()) -> m ()
syncer writer = do
  v <- liftIO $ atomically $ newEmptyTMVar
  writer (Syncer $ putTMVar v)
  mexc <- liftIO $ atomically $ readTMVar v
  for_ mexc (liftIO . throwIO)
