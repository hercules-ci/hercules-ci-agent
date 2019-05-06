module Hercules.Agent.Cachix.Info where

import           Protolude
import qualified Cachix.Client.Config          as Cachix.Config
import qualified Cachix.Client.Env             as Cachix.Env
import           Hercules.Agent.Log
import           Hercules.Agent.Cachix.Env
import qualified Katip.Core                    as K

getAvailablePushCaches :: (MonadReader r m, HasEnv r) => m [Text]
getAvailablePushCaches = do
  env <- asks (cachixEnv . getEnv)
  let
    pushableName Cachix.Config.BinaryCacheConfig { secretKey = _required, name = name }
      = [name]

  pure $ do -- []
    config <- toList $ Cachix.Env.config env
    cache <- toList $ Cachix.Config.binaryCaches config
    pushableName cache

activePushCaches :: (MonadReader r m, HasEnv r) => m [Text]
activePushCaches = asks (pushCaches . getEnv)

validate :: (MonadReader r m, HasEnv r, Katip m) => m ()
validate = do
  avc <- getAvailablePushCaches
  acc <- activePushCaches
  K.logLoc (K.sl "caches" acc) mempty InfoS $ "Configured caches: " <> K.logStr (show acc :: Text)
  forM_ (filter (`notElem` avc) acc) $ \c ->
    K.logLoc (K.sl "cache" c) mempty WarningS
      $ "Cachix push cache " <> K.logStr c <> "configured but not available. This may not work."
