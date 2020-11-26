{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hercules.Agent.Env where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as M
import Hercules.API.Agent.Socket.AgentPayload (AgentPayload)
import Hercules.API.Agent.Socket.ServicePayload (ServicePayload)
import qualified Hercules.Agent.Cachix.Env as Cachix
  ( Env,
    HasEnv (..),
  )
import Hercules.Agent.Config (FinalConfig)
import qualified Hercules.Agent.Config.BinaryCaches as Config.BinaryCaches
import qualified Hercules.Agent.Nix.Env as Nix
  ( Env,
  )
import qualified Hercules.Agent.ServiceInfo as ServiceInfo
import Hercules.Agent.Socket (Socket)
import Hercules.Error
import qualified Katip as K
import qualified Network.HTTP.Client
import Protolude
import qualified Servant.Auth.Client
import qualified Servant.Client.Streaming

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
    socket :: AgentSocket,
    -- katip
    kNamespace :: K.Namespace,
    kContext :: K.LogContexts,
    kLogEnv :: K.LogEnv
  }

activePushCaches :: App [Text]
activePushCaches = do
  bc <- asks (binaryCaches)
  pure $
    M.keys
      ( void (Config.BinaryCaches.cachixCaches bc)
          <> void (Config.BinaryCaches.nixCaches bc)
      )

type AgentSocket = Socket ServicePayload AgentPayload

instance Cachix.HasEnv Env where
  getEnv = cachixEnv

newtype App a = App {fromApp :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadCatch, MonadMask, MonadThrow, MonadUnliftIO, MonadBase IO, MonadBaseControl IO)

runApp :: Env -> App a -> IO a
runApp env (App m) = runReaderT m env

runHerculesClient ::
  NFData a =>
  (Servant.Auth.Client.Token -> Servant.Client.Streaming.ClientM a) ->
  App a
runHerculesClient f = do
  tok <- asks currentToken
  runHerculesClient' (f tok)

runHerculesClient' :: NFData a => Servant.Client.Streaming.ClientM a -> App a
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
