{-# LANGUAGE CPP #-}

module Hercules.Agent.Cachix
  ( module Hercules.Agent.Cachix,
    activePushCaches,
  )
where

import Cachix.Client.Push qualified as Cachix.Push
import Cachix.Types.BinaryCache (CompressionMethod (XZ))
#if MIN_VERSION_cachix(1,4,0) && ! MIN_VERSION_cachix(1,5,0)
import qualified Cachix.Client.Store
import qualified Cachix.Client.Store as Cachix
#endif
import Control.Monad.IO.Unlift
import Data.Conduit qualified as Conduit
import Data.Map qualified as M
import Hercules.Agent.Cachix.Env qualified as Agent.Cachix
import Hercules.Agent.Cachix.Info (activePushCaches)
import Hercules.Agent.Env as Agent.Env hiding (activePushCaches)
import Hercules.Agent.EnvironmentInfo qualified as EnvInfo
import Hercules.Agent.Log
import Hercules.CNix qualified as CNix
import Hercules.CNix.Store (StorePath)
import Hercules.Error
import Hercules.Formats.CachixCache qualified as CachixCache
import Protolude
#if MIN_VERSION_cachix(1,7,2)
import qualified Cachix.Client.OptionsParser as Opts
#endif

push :: CNix.Store -> Text -> [StorePath] -> Int -> App ()
push nixStore cache paths workers = withNamedContext "cache" cache $ do
  Agent.Cachix.Env
    { pushCaches = pushCaches,
      store = cachixStore,
      clientEnv = clientEnv
    } <-
    asks Agent.Cachix.getEnv
  pushCache <-
    escalate $
      maybeToEither (FatalError $ "Cache not found " <> cache) $
        M.lookup cache pushCaches
  ul <- askUnliftIO
  let pushParams = makePushParams pushCache cachixStore clientEnv
  paths' <- convertPaths nixStore paths
  void $
    Cachix.Push.pushClosure
      (\f l -> liftIO $ Cachix.Push.mapConcurrentlyBounded workers (fmap (unliftIO ul) f) l)
      pushParams
      paths'
  where
#if MIN_VERSION_cachix(1,6,0)
    makePushParams pushCache cachixStore clientEnv =
      Cachix.Push.PushParams
        { pushParamsName = Agent.Cachix.pushCacheName pushCache,
          pushParamsSecret = Agent.Cachix.pushCacheSecret pushCache,
          pushParamsStore = cachixStore,
          pushOnClosureAttempt = \_ missing -> return missing,
          pushParamsClientEnv = clientEnv,
          pushParamsStrategy = makePushStrategy
        }
#else
    makePushParams pushCache cachixStore clientEnv =
      Cachix.Push.PushParams
        { pushParamsName = Agent.Cachix.pushCacheName pushCache,
          pushParamsSecret = Agent.Cachix.pushCacheSecret pushCache,
          pushParamsStore = cachixStore,
          pushParamsClientEnv = clientEnv,
          pushParamsStrategy = makePushStrategy
        }
#endif

#if MIN_VERSION_cachix(1,7,2)
    makePushStrategy storePath =
      let ctx = makeContextForPath storePath
       in Cachix.Push.PushStrategy
            { onAlreadyPresent = pass,
              onAttempt = \retryStatus size ->
                ctx $
                  withNamedContext "size" size $
                    withNamedContext "retry" (show retryStatus :: Text) $
                      logLocM DebugS "pushing",
              on401 = makeOn401Handler,
              onError = \err -> throwIO $ FatalError $ "Error pushing to cachix: " <> show err,
              onDone = ctx $ logLocM DebugS "push done",
              compressionMethod = XZ,
              compressionLevel = 2,
              onUncompressedNARStream = \_ _ -> Conduit.awaitForever Conduit.yield,
              chunkSize = Opts.defaultChunkSize,
              numConcurrentChunks = Opts.defaultNumConcurrentChunks,
              omitDeriver = False
            }
#elif MIN_VERSION_cachix(1,6,0)
    makePushStrategy storePath =
      let ctx = makeContextForPath storePath
       in Cachix.Push.PushStrategy
            { onAlreadyPresent = pass,
              onAttempt = \retryStatus size ->
                ctx $
                  withNamedContext "size" size $
                    withNamedContext "retry" (show retryStatus :: Text) $
                      logLocM DebugS "pushing",
              on401 = makeOn401Handler,
              onError = \err -> throwIO $ FatalError $ "Error pushing to cachix: " <> show err,
              onDone = ctx $ logLocM DebugS "push done",
              compressionMethod = XZ,
              compressionLevel = 2,
              onUncompressedNARStream = \_ _ -> Conduit.awaitForever Conduit.yield,
              omitDeriver = False
            }
#elif MIN_VERSION_cachix(1,1,0)
    makePushStrategy storePath =
      let ctx = makeContextForPath storePath
       in Cachix.Push.PushStrategy
            { onAlreadyPresent = pass,
              onAttempt = \retryStatus size ->
                ctx $
                  withNamedContext "size" size $
                    withNamedContext "retry" (show retryStatus :: Text) $
                      logLocM DebugS "pushing",
              on401 = makeOn401Handler,
              onError = \err -> throwIO $ FatalError $ "Error pushing to cachix: " <> show err,
              onDone = ctx $ logLocM DebugS "push done",
              compressionMethod = XZ,
              compressionLevel = 2,
              omitDeriver = False
            }
#else
    makePushStrategy storePath =
      let ctx = makeContextForPath storePath
       in Cachix.Push.PushStrategy
            { onAlreadyPresent = pass,
              onAttempt = \retryStatus size ->
                ctx $
                  withNamedContext "size" size $
                    withNamedContext "retry" (show retryStatus :: Text) $
                      logLocM DebugS "pushing",
              on401 = makeOn401Handler,
              onError = \err -> throwIO $ FatalError $ "Error pushing to cachix: " <> show err,
              onDone = ctx $ logLocM DebugS "push done",
              withXzipCompressor = Cachix.Push.defaultWithXzipCompressor,
              omitDeriver = False
            }
#endif

#if MIN_VERSION_cachix(1,4,0) && ! MIN_VERSION_cachix(1,5,0)
    makeContextForPath storePath = withNamedContext "path" (Cachix.getPath storePath)
#else
    makeContextForPath storePath = withNamedContext "path" (show storePath :: Text)
#endif

#if MIN_VERSION_cachix(1,3,0)
    makeOn401Handler = \err -> throwIO $ FatalError $ "Cachix push is unauthorized: " <> show err
#else
    makeOn401Handler = throwIO $ FatalError "Cachix push is unauthorized"
#endif

#if MIN_VERSION_cachix(1,4,0) && ! MIN_VERSION_cachix(1,5,0)
    convertPaths nixStore paths = paths & traverse (convertPath nixStore)
#else
    convertPaths nixStore paths = do
      let _ = nixStore -- silence unused warning
      pure paths
#endif

#if MIN_VERSION_cachix(1,4,0) && ! MIN_VERSION_cachix(1,5,0)

-- This assumes that the store is not relocated, as cachix does not support that at the time of writing.
convertPath :: CNix.Store -> StorePath -> App Cachix.Client.Store.StorePath
convertPath store storePath = do
  storePathBytes <- liftIO $ CNix.storePathToPath store storePath
  let storePathText = decodeUtf8With lenientDecode storePathBytes
  pure (Cachix.Client.Store.StorePath storePathText)

#endif

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
