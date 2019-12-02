module Hercules.Agent.Nix.Init where

import Hercules.Agent.Nix.Env
import Protolude

newEnv :: IO Env
newEnv = do
  -- Might want to take stuff from Config and put it in
  -- extraOptions here.
  pure Env
    { extraOptions = []
    }
