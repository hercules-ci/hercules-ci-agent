module Hercules.Agent.Cachix.Env where

import qualified Cachix.Client.Push as Cachix
import Cachix.Client.Store (Store)
import Hercules.Formats.CachixCache (CachixCache)
import Protolude

data Env
  = Env
      { pushCaches :: Map Text Cachix.PushCache,
        cacheKeys :: Map Text CachixCache,
        netrcLines :: [Text],
        nixStore :: Store
        }

class HasEnv env where

  getEnv :: env -> Env

instance HasEnv Env where

  getEnv = identity
