{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Worker.STM where

import Control.Concurrent.Async
import Control.Concurrent.STM (TVar, readTVar, writeTVar)
import Control.Monad.Fix (mfix)
import Data.Map qualified as M
import Protolude

asyncIfSTM ::
  -- | Either return an existing async or return @b@ as an input to a new one.
  --
  -- The returned transaction must be not evaluate the 'Async' argument,
  -- but can write it somewhere.
  (Async a -> STM (Either (Async a) b)) ->
  -- | The action to run when the STM transaction decided to create a new 'Async'.
  (b -> IO a) ->
  -- | Either the async returned by the STM transaction, or a new async waiting
  -- for the provided IO.
  IO (Async a)
asyncIfSTM cond io = mfix \lazyAsync ->
  atomically (cond lazyAsync) >>= \case
    Left early -> pure early
    Right b -> do
      async (io b)

ensureTVarMapItem :: Ord k => k -> STM v -> TVar (Map k v) -> STM (Bool, v)
ensureTVarMapItem key mkValue mapVar = do
  map0 <- readTVar mapVar
  case M.lookup key map0 of
    Nothing -> do
      value <- mkValue
      writeTVar mapVar (M.insert key value map0)
      pure (False, value)
    Just value0 -> do
      pure (True, value0)

asyncInTVarMap :: Ord k => k -> TVar (Map k (Async a)) -> IO a -> IO (Async a)
asyncInTVarMap key mapVar action =
  asyncIfSTM
    ( \asy -> do
        (existed, v) <- ensureTVarMapItem key (pure asy) mapVar
        if existed
          then pure (Left v)
          else pure (Right ())
    )
    (\() -> action)
