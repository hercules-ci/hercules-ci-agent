module Hercules.Agent.Cachix.Env where

import           Protolude
import qualified Cachix.Client.Push as Cachix

data Env = Env
 { pushCaches :: Map Text Cachix.PushCache
 }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
