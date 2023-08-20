{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Memo
  ( Memo,
    newMemo,
    query,
    multiQuery,
  )
where

import Data.Map qualified as M
import Protolude hiding (atomically, bracket, throwIO)
import UnliftIO
  ( MonadUnliftIO,
    TVar,
    atomically,
    bracket,
    modifyTVar,
    newEmptyTMVarIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTVar,
    throwIO,
    writeTVar,
  )

data Entry v
  = Result v
  | Promise (STM (Either SomeException v))

-- | An unbounded cache
--
-- * 'MonadUnliftIO'
-- * process multiple keys at once
-- * anti dogpiling
-- * input not restricted to key, for efficiency
newtype Memo k v = Memo (TVar (Map k (Entry v)))

newMemo :: MonadIO m => m (Memo k v)
newMemo = liftIO do
  Memo <$> newTVarIO M.empty

query :: (Show k, Ord k, MonadUnliftIO m) => Memo k v -> (k -> m v) -> k -> m v
query memo handler k = do
  query'
    memo
    ( \m -> do
        unless (m == M.singleton k ()) do
          panic $ "query: key " <> show k <> " must be the only request"
        M.singleton k <$> handler k
    )
    k

query' :: (Show k, Ord k, MonadUnliftIO m) => Memo k v -> (Map k () -> m (Map k v)) -> k -> m v
query' memo handler k = do
  x <- multiQuery memo handler (M.singleton k ())
  case M.lookup k x of
    Nothing -> panic $ "query: key " <> show k <> " must occur in response"
    Just v -> pure v

-- | Perform a query such that no item is queried more than once.
multiQuery ::
  (Show k, Ord k, MonadUnliftIO m) =>
  Memo k v ->
  -- | Function to perform the query. This should always behave the same.
  (Map k () -> m (Map k v)) ->
  -- | The items to query
  Map k () ->
  -- | Deduplicated query action.
  m (Map k v)
multiQuery (Memo cacheVar) performQuery inputs = do
  -- Overview
  -- 1. Add promises to the map
  -- 2. Run the query for the new promises
  -- 3. Replace the promises by values so they're not removed by the bracket cleanup
  -- 4. Query the cache which now has the required entries
  multiQueryPromise <- newEmptyTMVarIO
  let makePromises = do
        M.mapWithKey \k _ -> Promise do
          mp <- readTMVar multiQueryPromise
          pure case M.lookup k mp of
            Nothing -> Left $ toException $ FatalError $ "multiQuery: input " <> show k <> " does not occur in query result"
            Just v -> Right v
      putPromises = do
        cache <- readTVar cacheVar
        let newInputs = M.difference inputs cache
            newPromises = makePromises newInputs
        writeTVar cacheVar (cache `M.union` newPromises)
        pure newPromises
      putResults rs = do
        cache <- readTVar cacheVar
        writeTVar cacheVar (fmap Result rs `M.union` cache)
      queryCache = do
        cache <- atomically $ readTVar cacheVar
        inputs & M.traverseWithKey \k _unit -> do
          case M.lookup k cache of
            Just (Result x) -> pure x
            Just (Promise x) -> do
              v' <- atomically x
              either throwIO pure v'
            Nothing -> panic $ "multiQuery: input " <> show k <> " was not saved to cache"
      clearPromises = \promises -> do
        modifyTVar cacheVar \c ->
          let cacheContainsPromise k _v = case M.lookup k c of
                Just (Promise _) -> True
                _ -> False
           in M.difference c (M.filterWithKey cacheContainsPromise promises)

  bracket
    (atomically putPromises)
    (atomically . clearPromises)
    \promises -> do
      when (not (null promises)) do
        response <- performQuery (void promises)
        atomically do
          putTMVar multiQueryPromise response
          putResults response
      queryCache
