module Hercules.Agent.Worker.Logging where

import Katip
import Protolude hiding (bracket)
import System.Posix (fdToHandle, stdError)
import System.Posix.IO (dup)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)

withKatip :: (MonadUnliftIO m) => Severity -> KatipContextT m a -> m a
withKatip s m = do
  let format :: forall a. (LogItem a) => ItemFormatter a
      format = (\_ _ _ -> "@katip ") <> jsonFormat
  -- Use a duplicate of stderr, to make sure we keep logging there, even after
  -- we reassign stderr to catch output from git and other subprocesses of Nix.
  dupStderr <- liftIO (fdToHandle =<< dup stdError)
  handleScribe <- liftIO $ mkHandleScribeWithFormatter format (ColorLog False) dupStderr (permitItem s) V2
  let makeLogEnv = registerScribe "stderr" handleScribe defaultScribeSettings =<< initLogEnv "Worker" "production"
      initialContext = ()
      extraNs = mempty -- "Worker" is already set in initLogEnv.
      -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
  bracket (liftIO makeLogEnv) (liftIO . closeScribes) $ \logEnv ->
    runKatipContextT logEnv initialContext extraNs m
