module Hercules.Error where

import Control.Monad.Catch
import Prelude

escalate :: (Exception exc, MonadThrow m) => Either exc a -> m a
escalate = escalateAs id

escalateAs :: (Exception exc, MonadThrow m) => (l -> exc) -> Either l a -> m a
escalateAs f = either (throwM . f) pure
