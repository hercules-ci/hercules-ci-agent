{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module Hercules.CLI.Login
  ( commandParser,
  )
where

import qualified Hercules.API.Accounts as Accounts
import Hercules.API.Accounts.CLIAuthorizationRequestCreate (CLIAuthorizationRequestCreate (CLIAuthorizationRequestCreate))
import qualified Hercules.API.Accounts.CLIAuthorizationRequestCreate as CLIAuthorizationRequestCreate
import qualified Hercules.API.Accounts.CLIAuthorizationRequestCreateResponse as CLIAuthorizationRequestCreateResponse
import qualified Hercules.API.Accounts.CLIAuthorizationRequestStatus as CLIAuthorizationRequestStatus
import Hercules.CLI.Client
import qualified Hercules.CLI.Credentials as Credentials
import Network.HostName (getHostName)
import qualified Options.Applicative as Optparse
import Protolude
import RIO (runRIO)
import System.Posix.User

commandParser :: Optparse.Parser (IO ())
commandParser = pure do
  hostname <- liftIO $ getHostName
  username <- getLoginName
  print hostname
  clientEnv <- Hercules.CLI.Client.init
  runRIO ((), clientEnv) do
    r <- runHerculesClient' do
      Accounts.postCLIAuthorizationRequest accountsClient CLIAuthorizationRequestCreate
        { description = toS username <> "@" <> toS hostname
        }
    putErrText $ "Please confirm your login at "
    putErrText $ "  " <> CLIAuthorizationRequestCreateResponse.browserURL r
    putErrText "Waiting for confirmation..."
    let tmpTok = CLIAuthorizationRequestCreateResponse.temporaryCLIToken r
        -- TODO do something pretty with 404
        pollLoop = do
          s <- runHerculesClient' do
            Accounts.getCLIAuthorizationRequestStatus accountsClient tmpTok
          case CLIAuthorizationRequestStatus.status s of
            CLIAuthorizationRequestStatus.Pending {} -> do
              liftIO (threadDelay 1_000_000)
              pollLoop
            CLIAuthorizationRequestStatus.Granted g -> pure g
    granted <- pollLoop
    domain <- liftIO Credentials.determineDomain
    liftIO (Credentials.writePersonalToken domain (CLIAuthorizationRequestStatus.token granted))
    for_ (CLIAuthorizationRequestStatus.userIdentities granted) \userIdentity ->
      putErrText $ "hci is configured to perform operations for " <> userIdentity <> " on " <> domain
