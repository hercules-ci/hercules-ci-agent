module Data.Conduit.Extras where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans
import Data.Conduit
import Prelude

-- | Write to a channel terminating with @Nothing@
sinkChan :: (MonadUnliftIO m, MonadIO m) => Chan (Maybe a) -> ConduitT a o m ()
sinkChan ch =
  handleC
    ( \e -> liftIO $ do
        writeChan ch Nothing
        throwIO (e :: SomeException)
    )
    $ do
      awaitForever $ \msg -> liftIO $ writeChan ch (Just msg)
      liftIO $ writeChan ch Nothing

-- | Read a channel until @Nothing@ is encountered
sourceChan :: MonadIO m => Chan (Maybe a) -> ConduitT i a m ()
sourceChan ch = do
  mmsg <- liftIO $ readChan ch
  case mmsg of
    Nothing -> liftIO $ writeChan ch Nothing
    Just msg -> yield msg >> sourceChan ch

conduitToCallbacks :: (MonadUnliftIO m, MonadIO m) => ConduitT () o m a -> (o -> m ()) -> m a
conduitToCallbacks c w = do
  runConduit (c `fuseUpstream` awaitForever (lift . w))
