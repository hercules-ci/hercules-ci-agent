{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.CLI.Client where

-- TODO https://github.com/haskell-servant/servant/issues/986

import Data.Has (Has, getter)
import qualified Data.Text as T
import Hercules.API (ClientAPI (..), ClientAuth, servantClientApi, useApi)
import Hercules.API.Accounts (AccountsAPI)
import Hercules.API.Projects (ProjectsAPI)
import Hercules.API.Repos (ReposAPI)
import Hercules.API.State (ContentDisposition, ContentLength, RawBytes, StateAPI)
import Hercules.Error
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (Status (statusCode, statusMessage))
import qualified Network.TLS as TLS
import Protolude
import RIO (RIO)
import Servant.API
import Servant.Auth.Client (Token)
import qualified Servant.Client
import Servant.Client.Core (ClientError, ResponseF)
import qualified Servant.Client.Core as Client
import qualified Servant.Client.Core.ClientError as ClientError
import Servant.Client.Generic (AsClientT)
import Servant.Client.Streaming (ClientM, responseStatusCode, showBaseUrl)
import qualified Servant.Client.Streaming
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

stateClient :: StateAPI ClientAuth (AsClientT ClientM)
stateClient = useApi clientState client

projectsClient :: ProjectsAPI ClientAuth (AsClientT ClientM)
projectsClient = useApi clientProjects client

reposClient :: ReposAPI ClientAuth (AsClientT ClientM)
reposClient = useApi clientRepos client

-- Duplicated from agent... create common lib?
determineDefaultApiBaseUrl :: IO Text
determineDefaultApiBaseUrl = do
  maybeEnv <- System.Environment.lookupEnv "HERCULES_CI_API_BASE_URL"
  pure $ maybe defaultApiBaseUrl toS maybeEnv

defaultApiBaseUrl :: Text
defaultApiBaseUrl = "https://hercules-ci.com"

newtype HerculesClientEnv = HerculesClientEnv Servant.Client.ClientEnv

newtype HerculesClientToken = HerculesClientToken Token

runHerculesClient :: (NFData a, Has HerculesClientToken r, Has HerculesClientEnv r) => (Token -> Servant.Client.Streaming.ClientM a) -> RIO r a
runHerculesClient f = do
  HerculesClientToken token <- asks getter
  runHerculesClient' $ f token

runHerculesClientEither :: (NFData a, Has HerculesClientToken r, Has HerculesClientEnv r) => (Token -> Servant.Client.Streaming.ClientM a) -> RIO r (Either Servant.Client.Streaming.ClientError a)
runHerculesClientEither f = do
  HerculesClientToken token <- asks getter
  runHerculesClientEither' $ f token

runHerculesClientStream ::
  (Has HerculesClientToken r, Has HerculesClientEnv r) =>
  (Token -> Servant.Client.Streaming.ClientM a) ->
  (Either Servant.Client.Streaming.ClientError a -> IO b) ->
  RIO r b
runHerculesClientStream f g = do
  HerculesClientToken token <- asks getter
  HerculesClientEnv clientEnv <- asks getter
  liftIO $ convertInternalError $ Servant.Client.Streaming.withClientM (f token) clientEnv g

runHerculesClient' :: (NFData a, Has HerculesClientEnv r) => Servant.Client.Streaming.ClientM a -> RIO r a
runHerculesClient' = runHerculesClientEither' >=> escalate

runHerculesClientEither' :: (NFData a, Has HerculesClientEnv r) => Servant.Client.Streaming.ClientM a -> RIO r (Either Servant.Client.Streaming.ClientError a)
runHerculesClientEither' m = do
  HerculesClientEnv clientEnv <- asks getter
  liftIO $ convertInternalError $ Servant.Client.Streaming.runClientM m clientEnv

init :: IO HerculesClientEnv
init = do
  manager <- Network.HTTP.Client.TLS.newTlsManager
  baseUrlText <- determineDefaultApiBaseUrl
  baseUrl <- Servant.Client.parseBaseUrl $ toS baseUrlText
  let clientEnv :: Servant.Client.ClientEnv
      clientEnv = Servant.Client.mkClientEnv manager baseUrl
  pure $ HerculesClientEnv clientEnv

dieWithHttpError :: Client.ClientError -> IO a
dieWithHttpError (Client.FailureResponse req resp) = do
  let status = responseStatusCode resp
      (base, path) = Client.requestPath req
  putErrText $
    "hci: Request failed; "
      <> show (statusCode status)
      <> " "
      <> decodeUtf8With lenientDecode (statusMessage status)
      <> " on: "
      <> toS (showBaseUrl base)
      <> "/"
      <> T.dropWhile (== '/') (decodeUtf8With lenientDecode path)
  liftIO exitFailure
dieWithHttpError e = do
  putErrText $ "hci: Request failed: " <> toS (displayException e)
  liftIO exitFailure

prettyPrintHttpErrors :: IO a -> IO a
prettyPrintHttpErrors = handle dieWithHttpError

-- | Low indicating the inclusiveness of the boundaries. Low is included. High is excluded.
-- A pair where `fst` > `snd` forms an empty range.
inLowRange :: (Ord a) => a -> (a, a) -> Bool
a `inLowRange` (p, q) = a >= p && a < q

-- In a library, this should support 429 with Retry-After
shouldRetryResponse :: Either ClientError r -> Bool
shouldRetryResponse (Left e) = shouldRetryClientError e
shouldRetryResponse _ = False

code :: ResponseF a -> Int
code = statusCode . responseStatusCode

shouldRetryClientError :: ClientError -> Bool
shouldRetryClientError (ClientError.FailureResponse _ resp) | code resp == 501 = False -- 501 Not Implemented
shouldRetryClientError (ClientError.FailureResponse _ resp) | code resp == 505 = False -- 505 HTTP Version Not Supported
shouldRetryClientError (ClientError.FailureResponse _ resp) | code resp == 408 = True -- 408 Request Timeout
shouldRetryClientError (ClientError.FailureResponse _ resp) | code resp `inLowRange` (500, 600) = True
shouldRetryClientError (ClientError.DecodeFailure _ _) = False -- Server programming error or API incompatibility
shouldRetryClientError (ClientError.UnsupportedContentType _ _) = False
shouldRetryClientError (ClientError.InvalidContentTypeHeader _) = False
shouldRetryClientError (ClientError.ConnectionError _) = True
shouldRetryClientError _ = False

-- | A custom exception type for HTTP exceptions that we consider retryable.
data HTTPInternalException = HTTPInternalException HTTP.Request SomeException
  deriving (Exception, Show)

-- | Convert a missed HTTP exception into a ClientError.
-- This is useful for retrying.
convertInternalError :: IO a -> IO a
convertInternalError = deescalateInternalError >=> escalate

deescalateInternalError :: IO a -> IO (Either ClientError a)
deescalateInternalError m = handleJust matchClientException (pure . Left) (Right <$> m)
  where
    matchClientException =
      fromException >=> \case
        HTTP.HttpExceptionRequest req (HTTP.InternalException e)
          | isJust (isRetryableHTTPInternalException e) ->
              Just $ ClientError.ConnectionError $ toException $ HTTPInternalException req $ toException e
        _ -> Nothing
    isRetryableHTTPInternalException =
      fromException >=> \e -> case e of
        TLS.Terminated _mysteryBool _msg tlsError
          | isRetryableTLSError tlsError ->
              pass
        _ -> Nothing
    -- Error_Protocol has been observed. Most others are probably also worth retrying.
    -- https://hackage.haskell.org/package/tls-1.9.0/docs/Network-TLS.html#t:TLSError
    -- real world example: Error_Protocol ("remote side fatal error",True,BadRecordMac))
    isRetryableTLSError =
      \case
        TLS.Error_Protocol {} -> True
        TLS.Error_EOF {} -> True
        TLS.Error_Packet {} -> True
        TLS.Error_Packet_unexpected {} -> True
        TLS.Error_Packet_Parsing {} -> True
        _ -> False

-- | ClientError printer that won't leak sensitive info.
clientErrorSummary :: ClientError -> Text
clientErrorSummary (ClientError.FailureResponse _ resp) = "status " <> show (responseStatusCode resp)
clientErrorSummary ClientError.DecodeFailure {} = "decode failure"
clientErrorSummary ClientError.UnsupportedContentType {} = "unsupported content type"
clientErrorSummary ClientError.InvalidContentTypeHeader {} = "invalid content type header"
clientErrorSummary (ClientError.ConnectionError e) = "connection error: " <> show e
