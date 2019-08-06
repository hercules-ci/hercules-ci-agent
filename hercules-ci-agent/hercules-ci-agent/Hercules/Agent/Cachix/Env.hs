module Hercules.Agent.Cachix.Env where

import           Protolude
import qualified Cachix.Client.Push            as Cachix
import           Hercules.Formats.CachixCache   ( CachixCache )
import           Cachix.Client.Store            ( Store )

data Env = Env
 { pushCaches :: Map Text Cachix.PushCache
 , cacheKeys :: Map Text CachixCache
 , netrcLines :: [Text]
 , nixStore :: Store
 }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
