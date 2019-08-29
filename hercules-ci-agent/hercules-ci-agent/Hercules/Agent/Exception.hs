module Hercules.Agent.Exception where

import qualified Control.Exception.Lifted
import qualified Control.Exception.Safe
import Control.Monad.Trans.Control (MonadBaseControl)
import Katip
import Katip.Monadic (logLocM)
import Protolude hiding (retry)

safeLiftedCatch :: MonadBaseControl IO m => m a -> (SomeException -> m a) -> m a
safeLiftedCatch m h =
  Control.Exception.Lifted.catch m
    $ \e ->
      if Control.Exception.Safe.isSyncException (e :: SomeException)
        then h e
        else Control.Exception.Lifted.throw e

safeLiftedHandle
  :: MonadBaseControl IO m
  => (SomeException -> m a)
  -> m a
  -> m a
safeLiftedHandle = flip safeLiftedCatch

exponential :: (Enum a, Floating a) => [a]
exponential = map exp [1, 2 ..]

cap :: Ord a => a -> [a] -> [a]
cap v = map (min v)

retry
  :: (KatipContext m, MonadBaseControl IO m)
  => [Double] -- ^ Seconds
  -> m a
  -> m a
retry delaysSeconds io = loop delaysSeconds
  where
    loop [] = io
    loop (delay : delays) = safeLiftedCatch io $ \e -> do
      logLocM WarningS $ "Retrying on exception: " <> logStr (show e :: Text)
      when (delay >= 0.000001) $ liftIO
        $ threadDelay
            (floor $ delay * 1000 * 1000)
      loop delays

-- | 5 minute exponential backoff
defaultRetry :: (KatipContext m, MonadBaseControl IO m) => m a -> m a
defaultRetry = retry (take 11 $ cap 60 exponential)
