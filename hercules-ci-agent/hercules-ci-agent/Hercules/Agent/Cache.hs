{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Cache where

import qualified Data.Map as M
import qualified Data.Text as T
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import qualified Hercules.Agent.Cachix as Cachix
import qualified Hercules.Agent.Config.BinaryCaches as Config
import Hercules.Agent.Env (App)
import qualified Hercules.Agent.Env as Env
import qualified Hercules.Agent.Netrc as Netrc
import qualified Hercules.Agent.Nix as Nix
import qualified Hercules.CNix as CNix
import Hercules.CNix.Std.Set (StdSet, toListFP)
import qualified Hercules.CNix.Std.Set as Std.Set
import Hercules.CNix.Store (StorePath)
import qualified Hercules.CNix.Store as Store
import Hercules.Error (defaultRetry)
import qualified Hercules.Formats.NixCache as NixCache
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
