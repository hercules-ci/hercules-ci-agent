module Hercules.Agent.Netrc.Env where

import Protolude

data Env = Env
  { netrcFile :: Maybe FilePath
  }
