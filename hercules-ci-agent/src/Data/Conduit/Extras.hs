module Data.Conduit.Extras where

import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Conduit
import Prelude

-- | Read a channel until @Nothing@ is encountered
sourceChan :: (MonadIO m) => Chan (Maybe a) -> ConduitT i a m ()
sourceChan ch = do
  mmsg <- liftIO $ readChan ch
  case mmsg of
    Nothing -> liftIO $ writeChan ch Nothing
    Just msg -> yield msg >> sourceChan ch

conduitToCallbacks :: (MonadIO m) => ConduitT () o m a -> (o -> m ()) -> m a
conduitToCallbacks c w = do
  runConduit (c `fuseUpstream` awaitForever (lift . w))
