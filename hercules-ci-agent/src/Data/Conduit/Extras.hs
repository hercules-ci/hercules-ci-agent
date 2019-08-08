module Data.Conduit.Extras where

import Prelude
import Data.Conduit
import Control.Monad.IO.Class
import Control.Concurrent.Chan

-- | Write to a channel terminating with @Nothing@
sinkChan :: MonadIO m => Chan (Maybe a) -> ConduitT a o m ()
sinkChan ch = do
  awaitForever $ \msg -> liftIO $ writeChan ch (Just msg)
  liftIO $ writeChan ch Nothing

-- | Read a channel until @Nothing@ is encountered
sourceChan :: MonadIO m => Chan (Maybe a) -> ConduitT i a m ()
sourceChan ch = do
  mmsg <- liftIO $ readChan ch
  case mmsg of
    Nothing -> liftIO $ writeChan ch Nothing
    Just msg -> yield msg >> sourceChan ch
