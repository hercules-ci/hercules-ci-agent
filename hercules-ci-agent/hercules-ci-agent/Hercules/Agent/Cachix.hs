module Hercules.Agent.Cachix
  ( module Hercules.Agent.Cachix,
    activePushCaches
    )
where

import qualified Cachix.Client.Push as Cachix.Push
import qualified Cachix.Client.URI as Cachix.URI
import Control.Monad.IO.Unlift
import qualified Data.Map as M
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Hercules.Agent.Cachix.Env as Agent.Cachix
import Hercules.Agent.Cachix.Info (activePushCaches)
import Hercules.Agent.Env as Agent.Env
import qualified Hercules.Agent.EnvironmentInfo as EnvInfo
import Hercules.Agent.Log
import qualified Hercules.Agent.Nix as Nix
import qualified Hercules.Agent.SecureDirectory as SecureDirectory
import Hercules.Error
import Protolude
import qualified Servant.Client as Servant
import System.IO (hClose)

push :: Text -> [Text] -> App ()
push cache paths = withNamedContext "cache" cache $ do
  Agent.Cachix.Env {pushCaches = pushCaches, nixStore = nixStore} <-
    asks $ Agent.Cachix.getEnv
  httpManager <- asks $ manager
  pushCache <-
    escalate
      $ maybeToEither (FatalError $ "Cache not found " <> cache)
      $ M.lookup cache pushCaches
  let clientEnv =
        Servant.mkClientEnv httpManager Cachix.URI.defaultCachixBaseUrl
  ul <- askUnliftIO
  void
    $ Cachix.Push.pushClosure
        ( \f l ->
            liftIO $ Cachix.Push.mapConcurrentlyBounded 4 (fmap (unliftIO ul) f) l
          )
        clientEnv
        nixStore
        pushCache
        ( \storePath ->
            let ctx = withNamedContext "path" storePath
             in Cachix.Push.PushStrategy
                  { onAlreadyPresent = pass,
                    onAttempt = \_retry -> ctx $ logLocM DebugS "pushing",
                    on401 = throwIO $ FatalError $ "Cachix push is unauthorized",
                    onError = \err -> throwIO $ FatalError $ "Error pushing to cachix: " <> show err,
                    onDone = ctx $ logLocM DebugS "push done",
                    withXzipCompressor = Cachix.Push.defaultWithXzipCompressor
                    }
          )
        paths

getNetrcLines :: App [Text]
getNetrcLines = asks (Agent.Cachix.netrcLines . Agent.Env.cachixEnv)

getSubstituters :: App [Text]
getSubstituters = do
  cks <- asks (Agent.Cachix.cacheKeys . Agent.Env.cachixEnv)
  nixInfo <- liftIO EnvInfo.getNixInfo
  pure
    ( EnvInfo.nixSubstituters nixInfo
        ++ map (\c -> "https://" <> c <> ".cachix.org") (M.keys cks)
      )

withCaches :: App a -> App a
withCaches m = do
  netrcLns <- getNetrcLines
  substs <- getSubstituters
  SecureDirectory.withSecureTempFile "tmp-netrc.key" $ \netrcPath netrcHandle -> do
    liftIO $ do
      Text.hPutStrLn netrcHandle (Text.unlines netrcLns)
      hClose netrcHandle
    Nix.withExtraOptions [("netrc-file", toSL netrcPath), ("substituters", Text.intercalate " " substs)] m
