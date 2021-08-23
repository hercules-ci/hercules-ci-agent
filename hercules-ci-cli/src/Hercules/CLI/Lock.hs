{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.CLI.Lock (commandParser) where

import Control.Monad.IO.Unlift (UnliftIO (UnliftIO), askUnliftIO)
import Control.Retry (RetryPolicyM, RetryStatus, capDelay, fullJitterBackoff, retrying, rsIterNumber)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Has (Has)
import Data.IORef (IORef)
import qualified Data.UUID
import qualified Data.UUID.V4 as UUID4
import Hercules.API (Id, NoContent)
import qualified Hercules.API.Accounts.SimpleAccount as SimpleAccount
import Hercules.API.Id (Id (Id), idText)
import qualified Hercules.API.Projects.Project as Project
import qualified Hercules.API.Projects.SimpleJob as SimpleJob
import Hercules.API.State (ProjectStateResourceGroup (acquireLock), StateAPI (deleteLockLease, updateLockLease))
import qualified Hercules.API.State.StateLockAcquireRequest as StateLockAcquireRequest
import Hercules.API.State.StateLockAcquireResponse (StateLockAcquireResponse (Acquired, Blocked))
import qualified Hercules.API.State.StateLockAcquireResponse as StateLockAcquireResponse
import qualified Hercules.API.State.StateLockLease as StateLockLease
import qualified Hercules.API.State.StateLockUpdateRequest as StateLockUpdateRequest
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, clientErrorSummary, determineDefaultApiBaseUrl, runHerculesClientEither, shouldRetryClientError, shouldRetryResponse, stateClient)
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Options (mkCommand)
import Hercules.CLI.Project (projectOption)
import Hercules.CLI.State (getProjectAndClient)
import Hercules.Error (escalate)
import Hercules.Frontend (mkLinks)
import qualified Hercules.Frontend
import qualified Network.URI
import Options.Applicative (help, long, metavar, strArgument, strOption, subparser)
import qualified Options.Applicative as Optparse
import Protolude
import RIO (RIO)
import Servant.Auth.Client (Token)
import Servant.Client.Core (ClientError)
import Servant.Client.Internal.HttpClient.Streaming (ClientM)
import qualified System.Environment
import qualified System.Process
import qualified UnliftIO
import UnliftIO.IORef (newIORef, readIORef, writeIORef)

commandParser, acquireCommandParser, releaseCommandParser, updateCommandParser, runCommandParser :: Optparse.Parser (IO ())
commandParser =
  subparser
    ( mkCommand
        "acquire"
        (Optparse.progDesc "Acquire a lock")
        acquireCommandParser
        <> mkCommand
          "update"
          (Optparse.progDesc "Refresh a lock timeout and/or description")
          updateCommandParser
        <> mkCommand
          "release"
          (Optparse.progDesc "Release a lock")
          releaseCommandParser
        <> mkCommand
          "run"
          (Optparse.progDesc "Run a command holding a lock")
          runCommandParser
    )
acquireCommandParser = do
  projectMaybe <- optional projectOption
  name <- nameOption
  json <- jsonOption
  description <- fromMaybe "hci lock acquire" <$> optional descriptionOption
  exclusive <- exclusiveOption
  wait_ <- waitOption
  pure do
    parent <- getLeaseIdFromEnv
    idempotencyKey <- Id <$> UUID4.nextRandom
    let request =
          StateLockAcquireRequest.StateLockAcquireRequest
            { description = description,
              exclusive = exclusive,
              parent = parent,
              idempotencyKey = Just idempotencyKey
            }
    runAuthenticated do
      projectStateClient <- getProjectAndClient projectMaybe
      let acquireReq = acquireLock projectStateClient name request
          onAcquire s = do
            putErrText "hci: lock acquired"
            if json
              then putLByteString (encodePretty s)
              else putText (idText $ StateLockAcquireResponse.leaseId s)
      if wait_
        then pollAcquire acquireReq >>= onAcquire
        else do
          ref <- newIORef Nothing
          tryAcquire ref acquireReq >>= \case
            Acquired s -> onAcquire s
            Blocked s -> do
              when json do
                putLByteString (encodePretty s)
              liftIO exitFailure
releaseCommandParser = do
  leaseId <- leaseIdOption
  pure do
    runAuthenticated do
      (_ :: NoContent) <- retryOnFail "lock release" (deleteLockLease stateClient leaseId)
      putErrText "hci: lock released"
updateCommandParser = do
  leaseId <- leaseIdOption
  descriptionUpdate <- optional descriptionOption
  json <- jsonOption
  pure do
    runAuthenticated do
      let request = StateLockUpdateRequest.StateLockUpdateRequest {description = descriptionUpdate}
      response <- retryOnFail "lock update" (updateLockLease stateClient leaseId request)
      when json do
        putLByteString (encodePretty response)
runCommandParser = do
  projectMaybe <- optional projectOption
  name <- nameOption
  descriptionMaybe <- optional descriptionOption
  exclusive <- exclusiveOption
  exe <- strArgument (metavar "COMMAND")
  args <- many (strArgument (metavar "ARGS"))
  pure do
    parent <- getLeaseIdFromEnv
    idempotencyKey <- Id <$> UUID4.nextRandom
    let request =
          StateLockAcquireRequest.StateLockAcquireRequest
            { description = description,
              exclusive = exclusive,
              parent = parent,
              idempotencyKey = Just idempotencyKey
            }
        description = fromMaybe ("hci lock run " <> toS exe) descriptionMaybe
    runAuthenticated do
      projectStateClient <- getProjectAndClient projectMaybe
      lease0 <- pollAcquire (acquireLock projectStateClient name request)
      putErrText "hci: lock acquired"
      let leaseId = StateLockAcquireResponse.leaseId lease0
      exitCode <-
        ( do
            env <- liftIO System.Environment.getEnvironment
            let procSpec = (System.Process.proc exe args) {System.Process.env = Just env'}
                env' = (leaseIdEnvVar, toS (idText leaseId)) : filter (\(k, _) -> k /= leaseIdEnvVar) env
                updateRequest =
                  StateLockUpdateRequest.StateLockUpdateRequest
                    { -- Not changing anything; just pinging
                      description = Nothing
                    }
                updateInterval = 3 * 60 * 1000 * 1000
                pinger = do
                  liftIO $ threadDelay updateInterval
                  forever do
                    ( do
                        (_ :: StateLockAcquireResponse.StateLockAcquiredResponse) <-
                          retryOnFail "lock pinger" do
                            updateLockLease stateClient leaseId updateRequest
                        liftIO $ threadDelay updateInterval
                      )
            UnliftIO unlift <- askUnliftIO
            liftIO do
              withAsync
                (unlift pinger)
                ( \_ -> do
                    (_, _, _, processHandle) <- System.Process.createProcess procSpec
                    System.Process.waitForProcess processHandle
                )
          )
          `UnliftIO.finally` do
            (_ :: NoContent) <- retryOnFail "lock release" (deleteLockLease stateClient leaseId)
            putErrText "hci: lock released"
      liftIO $ exitWith exitCode

simpleRetryPredicate :: Applicative m => (r -> Bool) -> RetryStatus -> r -> m Bool
simpleRetryPredicate f _rs r = pure (f r)

retryOnFail ::
  (NFData b, Has HerculesClientToken r, Has HerculesClientEnv r) =>
  Text ->
  (Token -> ClientM b) ->
  RIO r b
retryOnFail shortDesc req = escalate =<< retryOnFailEither shortDesc req

retryOnFailEither ::
  (NFData a, Has HerculesClientToken r, Has HerculesClientEnv r) =>
  Text ->
  (Token -> ClientM a) ->
  RIO r (Either ClientError a)
retryOnFailEither shortDesc req =
  retrying
    failureRetryPolicy
    (simpleRetryPredicate shouldRetryResponse)
    ( \rs -> do
        when (rsIterNumber rs /= 0) do
          liftIO $ putErrText $ "hci: " <> shortDesc <> " retrying."
        r <- runHerculesClientEither req
        for_ (leftToMaybe r) \e -> do
          liftIO $ putErrText $ "hci: " <> shortDesc <> " encountered " <> clientErrorSummary e <> "."
          when (shouldRetryClientError e) do
            liftIO $ putErrText $ "hci: " <> shortDesc <> " will retry."
        pure r
    )

-- NB: fullJitterBackoff is broken, https://github.com/Soostone/retry/issues/46
failureRetryPolicy :: MonadIO m => RetryPolicyM m
failureRetryPolicy = capDelay (120 * 1000 * 1000) (fullJitterBackoff 100000)

-- NB: fullJitterBackoff is broken, https://github.com/Soostone/retry/issues/46
waitRetryPolicy :: MonadIO m => RetryPolicyM m
waitRetryPolicy = capDelay (10 * 1000 * 1000) (fullJitterBackoff 500000)

tryAcquire ::
  (Has HerculesClientToken r, Has HerculesClientEnv r) =>
  IORef (Maybe StateLockAcquireResponse.StateLockBlockedResponse) ->
  (Token -> ClientM StateLockAcquireResponse) ->
  RIO r StateLockAcquireResponse
tryAcquire ref acquireLockRequest = do
  r <- retryOnFail "lock acquire" acquireLockRequest
  case r of
    Blocked s -> logBlockedMaybe ref s
    Acquired {} -> pass
  pure r

pollAcquire ::
  (Has HerculesClientToken r, Has HerculesClientEnv r) =>
  (Token -> ClientM StateLockAcquireResponse) ->
  RIO r StateLockAcquireResponse.StateLockAcquiredResponse
pollAcquire acquireLockRequest = do
  ref <- newIORef Nothing
  finalResp <-
    retrying
      waitRetryPolicy
      ( \_rs s -> case s of
          Blocked {} -> do
            putErrText "hci: waiting for lock..."
            pure True
          Acquired {} -> pure False
      )
      (const $ tryAcquire ref acquireLockRequest)
  case finalResp of
    Blocked {} -> panic "Retrying timed out" -- won't happen; policy is indefinite
    Acquired s -> pure s

logBlockedMaybe ::
  MonadIO m =>
  IORef (Maybe StateLockAcquireResponse.StateLockBlockedResponse) ->
  StateLockAcquireResponse.StateLockBlockedResponse ->
  m ()
logBlockedMaybe ref resp = do
  old <- readIORef ref
  when (old /= Just resp) do
    writeIORef ref (Just resp)
    logBlocked resp

logBlocked :: MonadIO m => StateLockAcquireResponse.StateLockBlockedResponse -> m ()
logBlocked s = do
  putErrText "hci: lock blocked"
  for_ (StateLockAcquireResponse.blockedByLeases s) \lease -> do
    putErrText "blocked by lease:"
    putErrText $ "  description: " <> StateLockLease.description lease
    for_ (StateLockLease.user lease) \user ->
      putErrText $ "  user: " <> SimpleAccount.displayName user <> " (" <> SimpleAccount.name user <> ")"
    for_ (StateLockLease.job lease) \job -> do
      baseUri <- liftIO getLinksBase
      let links = mkLinks baseUri
          project = SimpleJob.project job
          jobUrl =
            Hercules.Frontend.job
              links
              (Project.siteSlug project)
              (Project.ownerSlug project)
              (Project.slug project)
              (fromIntegral (SimpleJob.index job))
      putErrText $ "  job: " <> jobUrl

getLinksBase :: IO Network.URI.URI
getLinksBase = do
  url <- determineDefaultApiBaseUrl
  case Network.URI.parseAbsoluteURI (toS url) of
    Just x -> pure x
    Nothing -> panic "Could not parse API base url"

-- TODO: bytestring
leaseIdEnvVar :: [Char]
leaseIdEnvVar = "HERCULES_CI_LOCK_LEASE_ID"

getLeaseIdFromEnv :: IO (Maybe (Id "StateLockLease"))
getLeaseIdFromEnv = do
  strMaybe <- System.Environment.lookupEnv leaseIdEnvVar
  for strMaybe \str -> case Data.UUID.fromString str of
    Just x -> pure (Id x)
    Nothing -> do
      putErrLn (leaseIdEnvVar <> " is not a valid UUID")
      exitFailure

nameOption :: Optparse.Parser Text
nameOption = strOption $ long "name" <> metavar "NAME" <> help "Name of the lock"

jsonOption :: Optparse.Parser Bool
jsonOption = Optparse.flag False True (long "json" <> help "Write JSON results on stdout")

descriptionOption :: Optparse.Parser Text
descriptionOption = strOption $ long "description" <> metavar "TEXT" <> help "Describe the lock activity, for better messages"

-- NB: exclusive by default; inversion is contained
exclusiveOption :: Optparse.Parser Bool
exclusiveOption = Optparse.flag True False (long "non-exclusive" <> help "Acquire a non-exclusive lock aka read lock")

-- NB: wait by default; inversion is contained
waitOption :: Optparse.Parser Bool
waitOption = Optparse.flag True False (long "no-wait" <> help "Fail immediately when the lock is already taken")

leaseIdOption :: Optparse.Parser (Id "StateLockLease")
leaseIdOption = fmap Id $ Optparse.option Optparse.auto $ long "lease-id" <> metavar "UUID" <> help "Lease UUID"
