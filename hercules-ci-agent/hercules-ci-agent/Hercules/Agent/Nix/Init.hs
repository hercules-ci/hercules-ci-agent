module Hercules.Agent.Nix.Init where

import qualified Hercules.Agent.EnvironmentInfo as EnvironmentInfo
import Hercules.Agent.Nix.Env
import Protolude

newEnv :: IO Env
newEnv = do
  nixInfo <- EnvironmentInfo.getNixInfo
  when (EnvironmentInfo.nixNarinfoCacheNegativeTTL nixInfo /= Just 0) $ do
    putErrText
      "\n\
      \We have detected that the setting narinfo-cache-negative-ttl is non-zero.\n\
      \Running hercules-ci-agent on a system with a non-zero negative ttl will cause\n\
      \problems when run in a cluster.\n\
      \Note that this setting only affects the caching of paths that are *missing*\n\
      \from a cache. Paths that *are* in the binary cache are cached as configured in\n\
      \the 'positive' option.\n\
      \\n\
      \On NixOS and nix-darwin, use the recommended installation method via module,\n\
      \make sure that the `narinfo-cache-negative-ttl` isn't set via other means.\n\
      \If you can't use the module, use\
      \    nix.extraOptions = \"narinfo-cache-negative-ttl = 0\"\n\
      \\n\
      \or add to your system nix.conf:\n\
      \    narinfo-cache-negative-ttl = 0\n\
      \n"
    throwIO $ FatalError "Please configure your system's Nix with:  narinfo-cache-negative-ttl = 0  "
  -- Might want to take stuff from Config and put it in
  -- extraOptions here.
  pure Env
    { extraOptions = []
    }
