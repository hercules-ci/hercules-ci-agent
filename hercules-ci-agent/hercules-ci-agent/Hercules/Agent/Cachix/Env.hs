{-# LANGUAGE CPP #-}

module Hercules.Agent.Cachix.Env where

import Cachix.Client.Push qualified as Cachix
#if MIN_VERSION_cachix(1,4,0) && ! MIN_VERSION_cachix(1,5,0)
import Cachix.Client.Store qualified

#else
import Hercules.CNix.Store qualified

#endif
import Hercules.Formats.CachixCache (CachixCache)
import Protolude
import Servant.Client (ClientEnv)

#if MIN_VERSION_cachix(1,4,0) && ! MIN_VERSION_cachix(1,5,0)
type CachixStore = Cachix.Client.Store.Store

#else
type CachixStore = Hercules.CNix.Store.Store

#endif

data PushCache = PushCache
  { pushCacheName :: Text,
    pushCacheSecret :: Cachix.PushSecret
  }

data Env = Env
  { pushCaches :: Map Text PushCache,
    cacheKeys :: Map Text CachixCache,
    netrcLines :: [Text],
    store :: CachixStore,
    clientEnv :: ClientEnv
  }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
