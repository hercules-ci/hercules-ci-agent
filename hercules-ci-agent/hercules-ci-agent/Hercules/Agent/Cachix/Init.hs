{-# LANGUAGE DataKinds #-}
module Hercules.Agent.Cachix.Init where
import           Protolude

import qualified Cachix.Client.Push            as Cachix.Push
import qualified Cachix.Client.Secrets         as Cachix.Secrets
import           Hercules.Agent.Cachix.Env     as Env
import qualified Hercules.Formats.CachixCache   as CachixCache
import qualified Hercules.Agent.Config         as Config
import           Hercules.Error
import qualified Servant.Auth.Client

import qualified Katip                         as K
import qualified Data.Map                      as M

newEnv :: Config.FinalConfig -> Map Text CachixCache.CachixCache -> K.KatipContextT IO Env.Env
newEnv _config cks = do
  -- FIXME: sl doesn't work??
  K.katipAddContext (K.sl "caches" (M.keys cks)) $
    K.logLocM K.DebugS ("Cachix init " <> K.logStr (show (M.keys cks) :: Text))

  pcs <- liftIO $ toPushCaches cks

  pure Env.Env { pushCaches = pcs
               , netrcLines = toNetrcLines cks
               , cacheKeys = cks
               }

toNetrcLines :: Map Text CachixCache.CachixCache -> [Text]
toNetrcLines = concatMap toNetrcLine . M.toList where
  toNetrcLine (name, keys) = do
    pt <- toList $ CachixCache.authToken keys
    pure $ "machine " <> name <> ".cachix.org" <> " password " <> pt

toPushCaches :: Map Text CachixCache.CachixCache -> IO (Map Text Cachix.Push.PushCache)
toPushCaches = sequenceA . M.mapMaybeWithKey toPushCaches' where
  toPushCaches' name keys =
    let t = fromMaybe "" (CachixCache.authToken keys)
    in  do
          sk <- head $ CachixCache.signingKeys keys
          Just $ escalateAs FatalError $ do
            k' <- Cachix.Secrets.parseSigningKeyLenient sk
            pure $ Cachix.Push.PushCache
              { pushCacheName = name
              , pushCacheSigningKey = k'
              , pushCacheToken = Servant.Auth.Client.Token $ toSL t
              }
