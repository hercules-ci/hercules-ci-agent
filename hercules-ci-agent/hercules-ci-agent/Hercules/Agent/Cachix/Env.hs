module Hercules.Agent.Cachix.Env where

import qualified Cachix.Client.Push as Cachix
import Cachix.Client.Store (Store)
import Hercules.Formats.CachixCache (CachixCache)
import Protolude
import Servant.Client (ClientEnv)

data PushCache = PushCache
  { pushCacheName :: Text,
    pushCacheSecret :: Cachix.PushSecret
  }

data Env = Env
  { pushCaches :: Map Text PushCache,
    cacheKeys :: Map Text CachixCache,
    netrcLines :: [Text],
    nixStore :: Store,
    clientEnv :: ClientEnv
  }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
