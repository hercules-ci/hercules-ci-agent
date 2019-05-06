module Hercules.Agent.Cachix.Push where

import           Protolude
import qualified Cachix.Client.Commands          as Cachix.Commands
import           Hercules.Agent.Log
import           Hercules.Agent.Cachix.Env


push :: (MonadReader r m, HasEnv r, KatipContext m) => [Text] -> m ()
push paths = do
  env <- asks $ cachixEnv . getEnv
  caches <- asks $ pushCaches . getEnv
  forM_ caches $ \cache ->
    liftIO $ Cachix.Commands.push env cache paths False
