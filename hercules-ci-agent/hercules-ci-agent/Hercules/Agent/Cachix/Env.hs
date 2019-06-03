module Hercules.Agent.Cachix.Env where

import           Protolude
import qualified Cachix.Client.Push as Cachix
import           Cachix.Formats.CachixPublicKey (CachixPublicKey)

data Env = Env
 { pushCaches :: Map Text Cachix.PushCache
 , publicKeys :: [CachixPublicKey]
 , netrcLines :: [Text]
 }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = identity
