{-# LANGUAGE CPP #-}
module Hercules.Agent.Cachix
  ( module Hercules.Agent.Cachix,
    activePushCaches,
  )
where

import qualified Cachix.Client.Push as Cachix.Push
import Cachix.Types.BinaryCache (CompressionMethod(XZ))
import Control.Monad.IO.Unlift
import qualified Data.Map as M
import qualified Hercules.Agent.Cachix.Env as Agent.Cachix
import Hercules.Agent.Cachix.Info (activePushCaches)
import Hercules.Agent.Env as Agent.Env hiding (activePushCaches)
import qualified Hercules.Agent.EnvironmentInfo as EnvInfo
import Hercules.Agent.Log
import Hercules.CNix.Store (StorePath)
import Hercules.Error
import qualified Hercules.Formats.CachixCache as CachixCache
import Protolude

push :: Text -> [StorePath] -> Int -> App ()
push cache paths workers = withNamedContext "cache" cache $ do
  Agent.Cachix.Env
    { pushCaches = pushCaches,
      nixStore = nixStore,
      clientEnv = clientEnv
    } <-
    asks Agent.Cachix.getEnv
  pushCache <-
    escalate $
      maybeToEither (FatalError $ "Cache not found " <> cache) $
        M.lookup cache pushCaches
  ul <- askUnliftIO
  let pushParams =
        Cachix.Push.PushParams
          { pushParamsName = Agent.Cachix.pushCacheName pushCache,
            pushParamsSecret = Agent.Cachix.pushCacheSecret pushCache,
            pushParamsStore = nixStore,
            pushParamsClientEnv = clientEnv,
            pushParamsStrategy = \storePath ->
              let ctx = withNamedContext "path" (show storePath :: Text)
               in Cachix.Push.PushStrategy
                    { onAlreadyPresent = pass,
                      onAttempt = \retryStatus size ->
                        ctx $
                          withNamedContext "size" size $
                            withNamedContext "retry" (show retryStatus :: Text) $
                              logLocM DebugS "pushing",
#if MIN_VERSION_cachix(1,3,0)
                      on401 = \err -> throwIO $ FatalError $ "Cachix push is unauthorized: " <> show err,
#else
                      on401 = throwIO $ FatalError "Cachix push is unauthorized",
#endif
                      onError = \err -> throwIO $ FatalError $ "Error pushing to cachix: " <> show err,
                      onDone = ctx $ logLocM DebugS "push done",
#if MIN_VERSION_cachix(1,1,0)
                      compressionMethod = XZ,
                      compressionLevel = 2,
#else
                      withXzipCompressor = Cachix.Push.defaultWithXzipCompressor,
#endif
                      omitDeriver = False
                    }
          }
  void $
    Cachix.Push.pushClosure
      (\f l -> liftIO $ Cachix.Push.mapConcurrentlyBounded workers (fmap (unliftIO ul) f) l)
      pushParams
      paths

getNetrcLines :: App [Text]
getNetrcLines = asks (Agent.Cachix.netrcLines . Agent.Env.cachixEnv)

getSubstituters :: App [Text]
getSubstituters = do
  cks <- asks (Agent.Cachix.cacheKeys . Agent.Env.cachixEnv)
  nixInfo <- liftIO EnvInfo.getNixInfo
  pure
    ( map (decodeUtf8With lenientDecode) (EnvInfo.nixSubstituters nixInfo)
        ++ map (\c -> "https://" <> c <> ".cachix.org") (M.keys cks)
    )

getTrustedPublicKeys :: App [Text]
getTrustedPublicKeys = do
  cks <- asks (Agent.Cachix.cacheKeys . Agent.Env.cachixEnv)
  nixInfo <- liftIO EnvInfo.getNixInfo
  pure
    ( map (decodeUtf8With lenientDecode) (EnvInfo.nixTrustedPublicKeys nixInfo)
        ++ concatMap CachixCache.publicKeys cks
    )
