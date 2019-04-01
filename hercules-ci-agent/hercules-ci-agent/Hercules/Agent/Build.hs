module Hercules.Agent.Build where

import           Conduit
import           Protolude
import           System.Process
import qualified Hercules.Agent.Client
import           Hercules.Agent.Env
import           Hercules.Agent.Exception       ( defaultRetry )
import           Hercules.API                   ( noContent )
import           Hercules.API.Task              ( Task )
import qualified Hercules.API.Task             as Task
import qualified Hercules.API.Agent.Build.BuildEvent
                                               as BuildEvent
import qualified Hercules.API.Agent.Build.BuildTask
                                               as BuildTask
import qualified Hercules.API.Agent.Build      as API.Build
import           Data.Conduit.Process           ( sourceProcessWithStreams )
import           Hercules.Agent.Log
import qualified Data.Conduit.Combinators      as Conduit


performBuild :: Task BuildTask.BuildTask -> App ()
performBuild task = do
  buildTask <- defaultRetry $ runHerculesClient
    (API.Build.getBuild Hercules.Agent.Client.buildClient (Task.id task))

  let stdinc = pass
      stdoutc = pass -- FIXME: use
      stderrc = Conduit.fold
      procSpec = (System.Process.proc
                   "nix-store"
                   [ "--realise"
                   , "--timeout"
                   , "36000" -- 10h TODO: make configurable via meta.timeout and decrease default to 3600s or so
                   , "--max-silent-time"
                   , "1800" -- 0.5h TODO: make configurable via (?) and decrease default to 600s
                   , "--"
                   , toS $ BuildTask.derivationPath buildTask
                   ]
                 ) { close_fds = True -- Disable on Windows?
                   , cwd = Just "/"
                   }

  logLocM DebugS $ "Invoking nix-store: " <> show procSpec

  (status, _out, errBytes) <- liftIO $
    sourceProcessWithStreams procSpec stdinc stdoutc stderrc

  withNamedContext "exitStatus" (show status :: Text)
    $ logLocM DebugS
    $ "Returned from nix-store"

  noContent $ defaultRetry $ runHerculesClient $ API.Build.writeBuildLog
    Hercules.Agent.Client.buildClient
    (BuildTask.id buildTask)
    errBytes

  case status of
    ExitFailure e -> do
      withNamedContext "exitStatus" e $ logLocM ErrorS "Worker failed"

      let events :: [BuildEvent.BuildEvent]
          events = [BuildEvent.Done False]

      noContent $ defaultRetry $ runHerculesClient $ API.Build.updateBuild
        Hercules.Agent.Client.buildClient
        (BuildTask.id buildTask)
        events

      panic "Build failure" -- FIXME: this error is expected and should be handled normally
                            -- nonetheless it passes on to the tasks system

    ExitSuccess -> do
      logLocM DebugS $ "Clean nix-store exit"

      let events :: [BuildEvent.BuildEvent]
          events = [BuildEvent.Done True]

      noContent $ defaultRetry $ runHerculesClient $ API.Build.updateBuild
        Hercules.Agent.Client.buildClient
        (BuildTask.id buildTask)
        events
