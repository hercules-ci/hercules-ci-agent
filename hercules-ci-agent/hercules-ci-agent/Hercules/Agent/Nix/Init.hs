module Hercules.Agent.Nix.Init where

import Protolude

import Hercules.Agent.Nix.Env

newEnv :: IO Env
newEnv = do
  -- Might want to take stuff from Config and put it in
  -- extraOptions here.
  pure Env
    { extraOptions = []
    }
