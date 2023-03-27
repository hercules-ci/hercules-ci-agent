{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Worker.Conduit where

import Data.Conduit (ConduitT, await, awaitForever, yield, (.|))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Sequence qualified as Seq
import Protolude hiding (pred, yield)

tailC :: Monad m => Int -> ConduitT i i m ()
tailC n = do
  buf <- sinkTail n
  for_ buf yield

-- | Return the last @n@ items
sinkTail :: Monad m => Int -> ConduitT i o m (Seq i)
sinkTail n = do
  doBuffer mempty
  where
    doBuffer st =
      await >>= \case
        Nothing -> pure st
        Just item -> doBuffer $! (Seq.drop (length st - n + 1) st Seq.:|> item)

-- | Take at most @n@ items that satisfy the predicate, then stop consuming,
-- even if the next item does not match the predicate.
--
-- Return the number of counted messages and the total number of messages written.
takeCWhileStopEarly :: Monad m => (i -> Bool) -> Int -> ConduitT i i m (Int, Int)
takeCWhileStopEarly counts limit = go 0 0
  where
    go counted total | counted >= limit = pure (counted, total)
    go counted total =
      await >>= \case
        Nothing -> pure (counted, total)
        Just item -> do
          yield item
          if item & counts
            then go (counted + 1) (total + 1)
            else go counted (total + 1)

countProduction :: (Num n, MonadIO m) => (i -> Bool) -> IORef n -> ConduitT i i m ()
countProduction pred counter = awaitForever (\i -> increment i *> yield i)
  where
    increment i | pred i = liftIO $ modifyIORef counter (+ 1)
    increment _ = pass

withInputProductionCount :: MonadIO m => (i -> Bool) -> ConduitT i o m a -> ConduitT i o m (Int, a)
withInputProductionCount pred conduit = do
  counter <- liftIO $ newIORef 0
  r <- countProduction pred counter .| conduit
  (,r) <$> liftIO (readIORef counter)

withMessageLimit ::
  MonadIO m =>
  (a -> Bool) ->
  -- | First limit
  Int ->
  -- | Max tail part limit
  Int ->
  -- | What to do when truncatable output starts (waiting starts)
  ConduitT a a m () ->
  -- | What to do before yielding a truncated tail
  (Int -> ConduitT a a m ()) ->
  -- | What to do after yielding a truncated tail
  (Int -> ConduitT a a m ()) ->
  ConduitT a a m ()
withMessageLimit pred firstLimit tailLimit afterFirst beforeTail afterTail = do
  (c, _inclFlush) <- takeCWhileStopEarly pred firstLimit
  when (c == firstLimit) afterFirst
  (n, x) <- withInputProductionCount pred do
    sinkTail tailLimit
  let between = n - Seq.length x
  when (between > 0) do
    beforeTail between
  for_ x yield
  when (between > 0) do
    afterTail between
