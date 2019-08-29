module Hercules.Agent.Nix.Env where

import Protolude

data Env
  = Env
      { extraOptions :: [(Text, Text)]
        }
