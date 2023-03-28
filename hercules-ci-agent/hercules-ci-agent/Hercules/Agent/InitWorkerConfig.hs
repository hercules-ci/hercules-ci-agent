module Hercules.Agent.InitWorkerConfig where

import Data.Coerce (coerce)
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Env (App)
import Hercules.Agent.Env qualified
import Hercules.Agent.Nix qualified as Nix
import Hercules.Agent.WorkerProtocol.WorkerConfig (WorkerConfig (WorkerConfig))
import Hercules.Agent.WorkerProtocol.WorkerConfig qualified
import Hercules.CNix.Verbosity qualified as CNix.Verbosity
import Protolude

getWorkerConfig :: App WorkerConfig
getWorkerConfig = do
  verbosity_ <- asks (\env -> env.config.logLevel)
  nixVerbosity_ <- liftIO CNix.Verbosity.getVerbosity
  nixOptions_ <- Nix.askExtraOptions
  pure
    WorkerConfig
      { verbosity = coerce verbosity_,
        nixVerbosity = coerce nixVerbosity_,
        nixOptions = nixOptions_
      }
