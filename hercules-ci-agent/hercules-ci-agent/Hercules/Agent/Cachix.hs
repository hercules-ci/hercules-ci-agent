module Hercules.Agent.Cachix
  ( module Hercules.Agent.Cachix
  , activePushCaches
  )
where

import           Protolude
import qualified Cachix.Client.Push            as Cachix.Push
import qualified Cachix.Client.URI             as Cachix.URI
import           Control.Monad.IO.Unlift
import           Hercules.Error
import           Hercules.Agent.Log
import qualified Hercules.Agent.Cachix.Env     as Agent.Cachix
import           Hercules.Agent.Cachix.Info     ( activePushCaches )
import qualified Servant.Client                as Servant
import qualified Data.Map                      as M
import           Hercules.Agent.Env

push :: Text -> [Text] -> App ()
push cache paths = withNamedContext "cache" cache $ do
  Agent.Cachix.Env { pushCaches = pushCaches } <-
    asks $ Agent.Cachix.getEnv
  httpManager <- asks $ manager

  pushCache <-
    escalate
    $ maybeToEither (FatalError $ "Cache not found " <> cache)
    $ M.lookup cache pushCaches
  let clientEnv =
        Servant.mkClientEnv httpManager Cachix.URI.defaultCachixBaseUrl

  ul <- askUnliftIO

  void $ Cachix.Push.pushClosure
    (\f l ->
      liftIO $ Cachix.Push.mapConcurrentlyBounded 4 (fmap (unliftIO ul) f) l
    )
    clientEnv
    pushCache
    (\storePath ->
      let ctx = withNamedContext "path" storePath
      in  Cachix.Push.PushStrategy
            { onAlreadyPresent = pass
            , onAttempt = \_retry -> ctx $ logLocM DebugS "pushing"
            , on401 = panic "Cachix push is unauthorized"
            , onError = \err -> panic $ "Error pushing to cachix: " <> show err
            , onDone = ctx $ logLocM DebugS "push done"
            , withXzipCompressor = Cachix.Push.defaultWithXzipCompressor
            }
    )
    paths
