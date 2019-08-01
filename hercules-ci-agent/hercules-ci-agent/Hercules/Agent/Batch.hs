{-# LANGUAGE ScopedTypeVariables #-}
module Hercules.Agent.Batch where

import           Protolude

data StreamItem last a = Payload !a
                       | Flush (Maybe (MVar ()))
                       | Stop last

writePayload :: MonadIO m => Chan (StreamItem last a) -> a -> m ()
writePayload ch a = liftIO $ writeChan ch $ Payload a

endStream :: MonadIO m => Chan (StreamItem last a) -> last -> m ()
endStream ch a = liftIO $ writeChan ch $ Stop a

flushAsync :: MonadIO m => Chan (StreamItem last a) -> m ()
flushAsync ch = liftIO $ writeChan ch $ Flush Nothing

-- | Flush and wait for flush to happen with 1 minute timeout
flushSyncTimeout :: MonadIO m => Chan (StreamItem last a) -> m ()
flushSyncTimeout ch = liftIO $ do
  v <- newEmptyMVar
  withAsync
      (do
        threadDelay (1000 * 1000 * 60)
        tryPutMVar v ()
      )
    $ \_failsafe -> do
        writeChan ch $ Flush $ Just v
        readMVar v

boundedDelayBatcher  :: forall a b
                     . Int -- ^ Max time before flushing in microseconds
                    -> Int -- ^ Max number of items in batch
                    -> Chan (StreamItem () a) -- ^ Producer
                    -> ([a] -> IO ()) -- ^ Perform a batch
                    -> IO b -- ^ the work, producing items on the 'Chan'
                    -> IO b -- ^ start batcher, do the work, close and wait for the batcher
boundedDelayBatcher maxDelay maxItems chan performBatch m =
  boundedDelayBatcher' maxDelay maxItems chan performBatch (\batcherDone -> m <* endStream chan () <* wait batcherDone)

boundedDelayBatcher' :: forall last a b
                     . Int -- ^ Max time before flushing in microseconds
                    -> Int -- ^ Max number of items in batch
                    -> Chan (StreamItem last a) -- ^ Producer
                    -> ([a] -> IO ()) -- ^ Perform a batch
                    -> (Async last -> IO b)
                    -> IO b
boundedDelayBatcher' maxDelay maxItems chan performBatch m = do
  withAsync
      (forever $ do
        threadDelay $ maxDelay
        writeChan chan (Flush Nothing)
      )
    $ \_flusher -> do

        let beginReading :: IO last
            beginReading = readItems (max 1 maxItems) []

            doPerformBatch [] = pass
            doPerformBatch buf = performBatch (reverse buf)

            readItems :: Int -> [a] -> IO last
            readItems 0 buf = do
              doPerformBatch buf
              beginReading
            readItems bufferRemaining buf = readChan chan >>= \case
              Payload a -> readItems (bufferRemaining - 1) (a : buf)
              Flush mdone -> do
                doPerformBatch buf
                forM_ mdone $ \done -> putMVar done ()
                beginReading
              Stop last -> do
                doPerformBatch buf
                pure last

        withAsync beginReading $ \batcherAsync -> m batcherAsync
