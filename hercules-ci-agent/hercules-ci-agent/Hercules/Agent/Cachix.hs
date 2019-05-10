module Hercules.Agent.Cachix (
  module Hercules.Agent.Cachix
  , activePushCaches
  ) where

import           Protolude
import qualified Cachix.Client.Commands          as Cachix.Commands
import           Hercules.Agent.Log
import           Hercules.Agent.Cachix.Env
import           Hercules.Agent.Cachix.Info (activePushCaches)


push :: (MonadReader r m, HasEnv r, KatipContext m) => Text -> [Text] -> m ()
push cache paths = do
  env <- asks $ cachixEnv . getEnv
  liftIO $ Cachix.Commands.push env cache paths False
