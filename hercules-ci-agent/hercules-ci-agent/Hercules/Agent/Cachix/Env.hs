module Hercules.Agent.Cachix.Env where

import           Protolude
import qualified Cachix.Client.Push            as Cachix
import           Hercules.Formats.CachixCache    ( CachixCache )

data Env = Env
 { pushCaches :: Map Text Cachix.PushCache
 , cacheKeys :: Map Text CachixCache
 , netrcLines :: [Text]
 }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
