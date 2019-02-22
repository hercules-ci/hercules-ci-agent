module Hercules.Error where

import           Prelude
import           Control.Monad.Catch

escalate :: (Exception exc, MonadThrow m) => Either exc a -> m a
escalate = escalateAs id

escalateAs :: (Exception exc, MonadThrow m) => (l -> exc) -> Either l a -> m a
escalateAs f = either (throwM . f) pure
