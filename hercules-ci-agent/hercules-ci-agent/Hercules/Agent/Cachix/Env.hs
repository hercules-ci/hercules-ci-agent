module Hercules.Agent.Cachix.Env where

import           Protolude
import qualified Cachix.Client.Env             as Cachix.Env

data Env = Env
 { cachixEnv :: Cachix.Env.Env
 , pushCaches :: [Text]
 }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
