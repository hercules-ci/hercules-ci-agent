{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hercules.Agent.Env where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Map qualified as M
import Hercules.API.Agent.Socket.AgentPayload (AgentPayload)
import Hercules.API.Agent.Socket.ServicePayload (ServicePayload)
import Hercules.Agent.Cachix.Env qualified as Cachix
  ( Env,
    HasEnv (..),
  )
import Hercules.Agent.Config (FinalConfig)
import Hercules.Agent.Config.BinaryCaches qualified as Config.BinaryCaches
import Hercules.Agent.Memo (Memo)
import Hercules.Agent.Netrc.Env qualified as Netrc
import Hercules.Agent.Nix.Env qualified as Nix
  ( Env,
  )
import Hercules.Agent.ResourceLimiter (ResourceLimiter)
import Hercules.Agent.ServiceInfo qualified as ServiceInfo
import Hercules.Agent.Socket (Socket)
import Hercules.Error
import Katip qualified as K
import Network.HTTP.Client qualified
import Protolude
import Servant.Auth.Client qualified
import Servant.Client.Streaming qualified

data Env = Env
  { manager :: Network.HTTP.Client.Manager,
    config :: FinalConfig,
    herculesBaseUrl :: Servant.Client.Streaming.BaseUrl,
    herculesClientEnv :: Servant.Client.Streaming.ClientEnv,
    serviceInfo :: ServiceInfo.Env,
    -- TODO: The implicit limitation here is that we can
    --       only have one token at a time. I wouldn't be surprised if this becomes
    --       problematic at some point. Perhaps we should switch to a polymorphic
    --       reader monad like RIO when we hit that limitation.
    currentToken :: Servant.Auth.Client.Token,
    binaryCaches :: Config.BinaryCaches.BinaryCaches,
    cachixEnv :: Cachix.Env,
    nixEnv :: Nix.Env,
    netrcEnv :: Netrc.Env,
    socket :: AgentSocket,
    -- katip
    kNamespace :: K.Namespace,
    kContext :: K.LogContexts,
    kLogEnv :: K.LogEnv,
    -- | Limits concurrent store operations during evaluation. For use with caches.
    --
    -- May overlap with 'concurrentStorePushes' in practice, but the excess
    -- shouldn't be a problem. By having two counters we avoid starving either task.
    concurrentStoreQueries :: Memo Text ResourceLimiter,
    -- | Limits concurrent store operations during evaluation. For use with caches.
    --
    -- See 'concurrentStoreQueries'.
    concurrentStorePushes :: Memo Text ResourceLimiter
  }

activePushCaches :: App [Text]
activePushCaches = do
  bc <- asks binaryCaches
  pure $
    M.keys
      ( void (Config.BinaryCaches.cachixCaches bc)
          <> void (Config.BinaryCaches.nixCaches bc)
      )

type AgentSocket = Socket ServicePayload AgentPayload

instance Cachix.HasEnv Env where
  getEnv = cachixEnv

newtype App a = App {fromApp :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadCatch, MonadMask, MonadThrow, MonadUnliftIO, MonadBase IO, MonadBaseControl IO)

runApp :: Env -> App a -> IO a
runApp env (App m) = runReaderT m env

runHerculesClient ::
  (NFData a) =>
  (Servant.Auth.Client.Token -> Servant.Client.Streaming.ClientM a) ->
  App a
runHerculesClient f = do
  tok <- asks currentToken
  runHerculesClient' (f tok)

runHerculesClient' :: (NFData a) => Servant.Client.Streaming.ClientM a -> App a
runHerculesClient' m = do
  clientEnv <- asks herculesClientEnv
  escalate =<< liftIO (Servant.Client.Streaming.runClientM m clientEnv)

instance K.Katip App where
  getLogEnv = asks kLogEnv

  localLogEnv f (App m) = App (local (\s -> s {kLogEnv = f (kLogEnv s)}) m)

instance K.KatipContext App where
  getKatipContext = asks kContext

  localKatipContext f (App m) = App (local (\s -> s {kContext = f (kContext s)}) m)

  getKatipNamespace = asks kNamespace

  localKatipNamespace f (App m) = App (local (\s -> s {kNamespace = f (kNamespace s)}) m)
