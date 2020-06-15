{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Cache where

import qualified CNix
import qualified Cachix.Client.Store as Store
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import qualified Hercules.Agent.Cachix as Cachix
import qualified Hercules.Agent.Config.BinaryCaches as Config
import Hercules.Agent.Env (App)
import qualified Hercules.Agent.Env as Env
import qualified Hercules.Agent.Nix as Nix
import qualified Hercules.Agent.SecureDirectory as SecureDirectory
import qualified Hercules.Formats.NixCache as NixCache
import Katip
import Protolude
import System.IO (hClose)
import qualified Unsafe.Coerce

withCaches :: App a -> App a
withCaches m = do
  netrcLns <- Cachix.getNetrcLines
  csubsts <- Cachix.getSubstituters
  cpubkeys <- Cachix.getTrustedPublicKeys
  nixCaches <- asks (Config.nixCaches . Env.binaryCaches)
  let substs = nixCaches & toList <&> NixCache.storeURI
      pubkeys = nixCaches & toList <&> NixCache.publicKeys & join
  SecureDirectory.withSecureTempFile "tmp-netrc.key" $ \netrcPath netrcHandle -> do
    liftIO $ do
      T.hPutStrLn netrcHandle (T.unlines netrcLns)
      hClose netrcHandle
    Nix.withExtraOptions
      [ ("netrc-file", toSL netrcPath),
        ("substituters", T.intercalate " " (substs <> csubsts)),
        ("trusted-public-keys", T.intercalate " " (pubkeys <> cpubkeys))
      ]
      m

push ::
  -- | Cache name
  Text ->
  -- | Paths, which do not have to form a closure
  [Text] ->
  -- | Number of concurrent upload threads
  Int ->
  App ()
push cacheName paths concurrency = katipAddContext (sl "cacheName" cacheName) do
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

nixPush :: NixCache.NixCache -> [Text] -> Int -> App ()
nixPush cacheConf pathsText _concurrency = do
  let paths = map encodeUtf8 pathsText
  keys <- liftIO $ for (NixCache.signingKeys cacheConf) (CNix.parseSecretKey . encodeUtf8)
  CNix.withStore $ \store -> do
    (Sum total, Sum signed) <-
      mconcat <$> for keys \key -> liftIO $ do
        pathSet <- paths & map toS & newPathSetWith
        signClosure store key pathSet
    katipAddContext (sl "num-signatures" signed <> sl "num-paths" total) $
      logLocM DebugS "Signed"
    liftIO $ CNix.clearPathInfoCache store
    CNix.withStoreFromURI (NixCache.storeURI cacheConf) $ \cache -> do
      liftIO (CNix.copyClosure store cache paths)

newPathSetWith :: [ByteString] -> IO Store.PathSet
newPathSetWith paths = do
  pathSet <- Store.newEmptyPathSet
  for_ paths \path ->
    Store.addToPathSet path pathSet
  pure pathSet

castStore :: Ptr (CNix.Ref CNix.NixStore) -> Store.Store
castStore = Unsafe.Coerce.unsafeCoerce

signClosure :: Ptr (CNix.Ref CNix.NixStore) -> ForeignPtr CNix.SecretKey -> Store.PathSet -> IO (Sum Int, Sum Int)
signClosure store key' pathSet = withForeignPtr key' \key -> do
  closure <- Store.computeFSClosure (castStore store) Store.defaultClosureParams pathSet
  closure
    & Store.traversePathSet
      ( \path -> do
          CNix.signPath store key path
      )
    <&> foldMap (\case True -> (1, 1); False -> (1, 0))
