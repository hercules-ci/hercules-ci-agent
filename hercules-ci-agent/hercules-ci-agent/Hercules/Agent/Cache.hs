{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Cache where

import Data.Map qualified as M
import Data.Text qualified as T
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Hercules.Agent.Cachix qualified as Cachix
import Hercules.Agent.Config.BinaryCaches qualified as Config
import Hercules.Agent.Env (App)
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.Netrc qualified as Netrc
import Hercules.Agent.Nix qualified as Nix
import Hercules.CNix qualified as CNix
import Hercules.CNix.Std.Set (StdSet, toListFP)
import Hercules.CNix.Std.Set qualified as Std.Set
import Hercules.CNix.Store (StorePath)
import Hercules.CNix.Store qualified as Store
import Hercules.Error (defaultRetry)
import Hercules.Formats.NixCache qualified as NixCache
import Katip
import Protolude

withCaches :: App a -> App a
withCaches m = do
  netrcLns <- Cachix.getNetrcLines
  csubsts <- Cachix.getSubstituters
  cpubkeys <- Cachix.getTrustedPublicKeys
  nixCaches <- asks (Config.nixCaches . Env.binaryCaches)
  let substs = nixCaches & toList <&> NixCache.storeURI
      pubkeys = nixCaches & toList <&> NixCache.publicKeys & join
  netrcFile <- Netrc.getNetrcFile
  Netrc.appendLines netrcLns
  Nix.withExtraOptions
    [ ("netrc-file", toS netrcFile),
      ("substituters", T.intercalate " " (substs <> csubsts)),
      ("trusted-public-keys", T.intercalate " " (pubkeys <> cpubkeys))
    ]
    m

push ::
  -- | Cache name
  Text ->
  -- | Paths, which do not have to form a closure
  [StorePath] ->
  -- | Number of concurrent upload threads
  Int ->
  App ()
push cacheName paths concurrency = katipAddNamespace "Push" $ katipAddContext (sl "cacheName" cacheName) do
  caches <- asks Env.binaryCaches
  let maybeNix =
        Config.nixCaches caches & M.lookup cacheName & fmap \cache ->
          nixPush cache paths concurrency
      maybeCachix =
        Config.cachixCaches caches & M.lookup cacheName & fmap \_cache ->
          Cachix.push cacheName paths concurrency
      failNothing =
        throwIO $ FatalError $ "Agent does not have a binary cache named " <> show cacheName <> " in its configuration."
  fromMaybe failNothing (maybeNix <|> maybeCachix)

nixPush :: NixCache.NixCache -> [StorePath] -> Int -> App ()
nixPush cacheConf paths _concurrency = do
  keys <- liftIO $ for (NixCache.signingKeys cacheConf) (CNix.parseSecretKey . encodeUtf8)
  CNix.withStore $ \store -> do
    (Sum total, Sum signed) <- liftIO do
      pathSet <- Std.Set.fromListFP paths
      mconcat <$> for keys \key -> do
        signClosure store key pathSet
    katipAddContext (sl "num-signatures" signed <> sl "num-paths" total) $
      logLocM DebugS "Signed"
    liftIO $ CNix.clearPathInfoCache store
    defaultRetry . CNix.withStoreFromURI (NixCache.storeURI cacheConf) $ \cache -> do
      liftIO (CNix.copyClosure store cache paths)

signClosure :: CNix.Store -> ForeignPtr CNix.SecretKey -> StdSet Store.NixStorePath -> IO (Sum Int, Sum Int)
signClosure store key' pathSet = withForeignPtr key' \key -> do
  closure <- Store.computeFSClosure store Store.defaultClosureParams pathSet >>= toListFP
  closure
    & traverse
      ( \path -> do
          CNix.signPath store key path
      )
    <&> foldMap (\case True -> (1, 1); False -> (1, 0))
