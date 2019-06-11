module Hercules.Agent.Cachix.Env where

import           Protolude
import qualified Cachix.Client.Push as Cachix
import           Hercules.Formats.CacheKeys (CacheKeys)

data Env = Env
 { pushCaches :: Map Text Cachix.PushCache
 , cacheKeys :: CacheKeys
 , netrcLines :: [Text]
 }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
