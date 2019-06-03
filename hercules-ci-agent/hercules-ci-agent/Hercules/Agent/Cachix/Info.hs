module Hercules.Agent.Cachix.Info where

import           Protolude
import           Hercules.Agent.Cachix.Env
import qualified Data.Map                     as M

activePushCaches :: (MonadReader r m, HasEnv r) => m [Text]
activePushCaches = asks (M.keys . pushCaches . getEnv)
