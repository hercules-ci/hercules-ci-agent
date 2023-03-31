{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Hercules.Agent.Cachix.Init where

#if MIN_VERSION_cachix(0,7,0)
import Cachix.Client.Version (cachixVersion)
#else
import Cachix.Client.Env (cachixVersion)
#endif
import Cachix.Client.Push qualified as Cachix.Push
import Cachix.Client.Secrets qualified as Cachix.Secrets
import Cachix.Client.URI (defaultCachixBaseUrl)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map qualified as M
import Hercules.Agent.Cachix.Env as Env
import Hercules.Agent.Config qualified as Config
import Hercules.CNix.Store (openStore)
import Hercules.Error
import Hercules.Formats.CachixCache qualified as CachixCache
import Katip (KatipContext)
import Katip qualified as K
import Network.HTTP.Client (ManagerSettings (managerModifyRequest, managerResponseTimeout), responseTimeoutNone)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Simple (setRequestHeader)
import Protolude
import Servant.Auth.Client qualified
import Servant.Client (mkClientEnv)

-- TODO use from lib after cachix >0.3.5 + https://github.com/cachix/cachix/pull/274
customManagerSettings :: ManagerSettings
customManagerSettings =
  tlsManagerSettings
    { managerResponseTimeout = responseTimeoutNone,
      -- managerModifyRequest :: Request -> IO Request
      managerModifyRequest = return . setRequestHeader "User-Agent" [encodeUtf8 cachixVersion]
    }

withEnv :: (MonadUnliftIO m, KatipContext m) => Config.FinalConfig -> Map Text CachixCache.CachixCache -> (Env.Env -> m a) -> m a
withEnv _config cks continue = do
  -- FIXME: sl doesn't work??
  K.katipAddContext (K.sl "caches" (M.keys cks)) $
    K.logLocM K.DebugS ("Cachix init " <> K.logStr (show (M.keys cks) :: Text))
  pcs <- liftIO $ toPushCaches cks
  httpManager <- newTlsManagerWith customManagerSettings
  env <- do
    store <- liftIO openStore
    pure
      Env.Env
        { pushCaches = pcs,
          netrcLines = toNetrcLines cks,
          cacheKeys = cks,
          nixStore = store,
          clientEnv = mkClientEnv httpManager defaultCachixBaseUrl
        }
  continue env

toNetrcLines :: Map Text CachixCache.CachixCache -> [Text]
toNetrcLines = concatMap toNetrcLine . M.toList
  where
    toNetrcLine (name, keys) = do
      pt <- toList $ CachixCache.authToken keys
      pure $ "machine " <> name <> ".cachix.org" <> " login authtoken password " <> pt

toPushCaches :: Map Text CachixCache.CachixCache -> IO (Map Text PushCache)
toPushCaches = sequenceA . M.mapMaybeWithKey toPushCaches'
  where
    toPushCaches' name keys =
      let t = fromMaybe "" (CachixCache.authToken keys)
       in do
            sk <- head $ CachixCache.signingKeys keys
            Just $
              escalateAs FatalError $ do
                k' <- Cachix.Secrets.parseSigningKeyLenient sk
                pure
                  PushCache
                    { pushCacheName = name,
                      pushCacheSecret =
                        Cachix.Push.PushSigningKey
                          (Servant.Auth.Client.Token $ encodeUtf8 t)
                          k'
                    }
            <|> do
              token <- head $ CachixCache.authToken keys
              Just $
                pure
                  PushCache
                    { pushCacheName = name,
                      pushCacheSecret =
                        Cachix.Push.PushToken (Servant.Auth.Client.Token $ encodeUtf8 token)
                    }
