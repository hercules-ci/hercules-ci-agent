module Hercules.Agent.Cachix.Info where

import Data.Map qualified as M
import Hercules.Agent.Cachix.Env
import Protolude

activePushCaches :: (MonadReader r m, HasEnv r) => m [Text]
activePushCaches = asks (M.keys . pushCaches . getEnv)
