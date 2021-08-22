{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.CLI.Lock (commandParser) where

import Control.Monad.IO.Unlift (UnliftIO (UnliftIO), askUnliftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.UUID
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
import Hercules.CLI.Client (determineDefaultApiBaseUrl, runHerculesClient, stateClient)
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Options (mkCommand)
import Hercules.CLI.Project (projectOption)
import Hercules.CLI.State (getProjectAndClient)
import Hercules.Frontend (mkLinks)
import qualified Hercules.Frontend
import qualified Network.URI
import Options.Applicative (help, long, metavar, strArgument, strOption, subparser)
import qualified Options.Applicative as Optparse
import Protolude
import qualified System.Environment
import qualified System.Process
import qualified UnliftIO

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
  pure do
    parent <- getLeaseIdFromEnv
    let request =
          StateLockAcquireRequest.StateLockAcquireRequest
            { description = description,
              exclusive = exclusive,
              parent = parent
            }
    runAuthenticated do
      projectStateClient <- getProjectAndClient projectMaybe
      runHerculesClient (acquireLock projectStateClient name request) >>= \case
        Acquired s -> do
          putErrText "hci: lock acquired"
          if json
            then putLByteString (encodePretty s)
            else putText (idText $ StateLockAcquireResponse.leaseId s)
        Blocked s -> do
          putErrText "hci: lock blocked"
          for_ (StateLockAcquireResponse.blockedByLeases s) \lease -> do
            putErrText "blocked by lease:"
            putErrText $ "  description: " <> StateLockLease.description lease
            for_ (StateLockLease.user lease) \user ->
              putErrText $ "  user: " <> SimpleAccount.displayName user <> " (" <> SimpleAccount.name user <> ")"
            for_ (StateLockLease.job lease) \job -> do
              baseUrl <- liftIO getLinksBase
              let links = mkLinks baseUrl
                  project = SimpleJob.project job
                  jobUrl =
                    Hercules.Frontend.job
                      links
                      (Project.siteSlug project)
                      (Project.ownerSlug project)
                      (Project.slug project)
                      (fromIntegral (SimpleJob.index job))
              putErrText $ "  job: " <> jobUrl
          when json do
            putLByteString (encodePretty s)
          liftIO exitFailure
releaseCommandParser = do
  leaseId <- leaseIdOption
  pure do
    runAuthenticated do
      (_ :: NoContent) <- runHerculesClient (deleteLockLease stateClient leaseId)
      putErrText "hci: lock released"
updateCommandParser = do
  leaseId <- leaseIdOption
  descriptionUpdate <- optional descriptionOption
  json <- jsonOption
  pure do
    runAuthenticated do
      let request = StateLockUpdateRequest.StateLockUpdateRequest {description = descriptionUpdate}
      response <- runHerculesClient (updateLockLease stateClient leaseId request)
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
    let request =
          StateLockAcquireRequest.StateLockAcquireRequest
            { description = description,
              exclusive = exclusive,
              parent = parent
            }
        description = fromMaybe ("hci lock run " <> toS exe) descriptionMaybe
    runAuthenticated do
      projectStateClient <- getProjectAndClient projectMaybe
      lease0 <-
        runHerculesClient (acquireLock projectStateClient name request) >>= \case
          Acquired s -> do
            putErrText "hci: lock acquired"
            pure s
          Blocked s -> do
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
            liftIO exitFailure
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
                regularInterval = 3 * 60 * 1000 * 1000
                retryInterval = 10 * 1000 * 1000
                pinger = do
                  liftIO $ threadDelay regularInterval
                  forever do
                    ( do
                        runHerculesClient (void . updateLockLease stateClient leaseId updateRequest)
                        liftIO $ threadDelay regularInterval
                      )
                      `UnliftIO.catch` \e -> do
                        liftIO $ putErrText $ "hci: lock pinger encountered error. If this persists for minutes, lock will be lost. " <> show (e :: SomeException)
                        liftIO $ threadDelay retryInterval

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
            (_ :: NoContent) <- runHerculesClient (deleteLockLease stateClient leaseId)
            putErrText "hci: lock released"
      liftIO $ exitWith exitCode

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

leaseIdOption :: Optparse.Parser (Id "StateLockLease")
leaseIdOption = fmap Id $ Optparse.option Optparse.auto $ long "lease-id" <> metavar "UUID" <> help "Lease UUID"
