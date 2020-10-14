{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.CLI.Client where

-- TODO https://github.com/haskell-servant/servant/issues/986

import Data.Has (Has, getter)
import Hercules.API (ClientAPI (..), ClientAuth, servantClientApi, useApi)
import Hercules.API.Accounts (AccountsAPI)
import Hercules.API.State (ContentDisposition, ContentLength, RawBytes)
import Hercules.Error
import qualified Network.HTTP.Client.TLS
import Protolude
import RIO (RIO)
import Servant.API
import Servant.API.Generic
import Servant.Auth.Client (Token)
import qualified Servant.Client
import Servant.Client.Generic (AsClientT)
import qualified Servant.Client.Streaming
import Servant.Client.Streaming (ClientM)
import qualified System.Environment

-- | Bad instance to make it the client for State api compile. GHC seems to pick
-- the wrong overlappable instance.
instance
  FromSourceIO
    RawBytes
    ( Headers
        '[ContentLength, ContentDisposition]
        (SourceIO RawBytes)
    )
  where
  fromSourceIO = addHeader (-1) . addHeader "" . fromSourceIO

client :: ClientAPI ClientAuth (AsClientT ClientM)
client = fromServant $ Servant.Client.Streaming.client (servantClientApi @ClientAuth)

accountsClient :: AccountsAPI ClientAuth (AsClientT ClientM)
accountsClient = useApi clientAccounts client

-- Duplicated from agent... create common lib?
determineDefaultApiBaseUrl :: IO Text
determineDefaultApiBaseUrl = do
  maybeEnv <- System.Environment.lookupEnv "HERCULES_CI_API_BASE_URL"
  pure $ maybe defaultApiBaseUrl toS maybeEnv

defaultApiBaseUrl :: Text
defaultApiBaseUrl = "https://hercules-ci.com"

newtype HerculesClientEnv = HerculesClientEnv Servant.Client.ClientEnv

newtype HerculesClientToken = HerculesClientToken Token

runHerculesClient' :: (NFData a, Has HerculesClientEnv r) => Servant.Client.Streaming.ClientM a -> RIO r a
runHerculesClient' m = do
  HerculesClientEnv clientEnv <- asks getter
  escalate =<< liftIO (Servant.Client.Streaming.runClientM m clientEnv)

init :: IO HerculesClientEnv
init = do
  manager <- Network.HTTP.Client.TLS.newTlsManager
  baseUrlText <- determineDefaultApiBaseUrl
  baseUrl <- Servant.Client.parseBaseUrl $ toS $ baseUrlText
  let clientEnv :: Servant.Client.ClientEnv
      clientEnv = Servant.Client.mkClientEnv manager baseUrl
  pure $ HerculesClientEnv clientEnv
