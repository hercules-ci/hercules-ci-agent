{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hercules.Agent.Env where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Hercules.Agent.Cachix.Env as Cachix
  ( Env,
    HasEnv (..)
    )
import Hercules.Agent.Config (FinalConfig)
import qualified Hercules.Agent.Nix.Env as Nix
  ( Env
    )
import Hercules.Error
import qualified Katip as K
import qualified Network.HTTP.Client
import Protolude
import qualified Servant.Auth.Client
import qualified Servant.Client

data Env
  = Env
      { manager :: Network.HTTP.Client.Manager,
        config :: FinalConfig,
        herculesBaseUrl :: Servant.Client.BaseUrl,
        herculesClientEnv :: Servant.Client.ClientEnv,
        -- TODO: The implicit limitation here is that we can
        --       only have one token at a time. I wouldn't be surprised if this becomes
        --       problematic at some point. Perhaps we should switch to a polymorphic
        --       reader monad like RIO when we hit that limitation.
        currentToken :: Servant.Auth.Client.Token,
        cachixEnv :: Cachix.Env,
        nixEnv :: Nix.Env,
        -- katip
        kNamespace :: K.Namespace,
        kContext :: K.LogContexts,
        kLogEnv :: K.LogEnv
        }

instance Cachix.HasEnv Env where

  getEnv = cachixEnv

newtype App a = App {fromApp :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadCatch, MonadMask, MonadThrow, MonadUnliftIO, MonadBase IO, MonadBaseControl IO)

runApp :: Env -> App a -> IO a
runApp env (App m) = runReaderT m env

runHerculesClient
  :: (Servant.Auth.Client.Token -> Servant.Client.ClientM a)
  -> App a
runHerculesClient f = do
  tok <- asks currentToken
  runHerculesClient' (f tok)

runHerculesClient' :: Servant.Client.ClientM a -> App a
runHerculesClient' m = do
  clientEnv <- asks herculesClientEnv
  escalate =<< liftIO (Servant.Client.runClientM m clientEnv)

instance K.Katip App where

  getLogEnv = asks kLogEnv

  localLogEnv f (App m) = App (local (\s -> s {kLogEnv = f (kLogEnv s)}) m)

instance K.KatipContext App where

  getKatipContext = asks kContext

  localKatipContext f (App m) = App (local (\s -> s {kContext = f (kContext s)}) m)

  getKatipNamespace = asks kNamespace

  localKatipNamespace f (App m) = App (local (\s -> s {kNamespace = f (kNamespace s)}) m)
