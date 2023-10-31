module Hercules.Agent.WorkerProcess
  ( runWorker,
    getWorkerExe,
    getDaemonExe,
    WorkerEnvSettings (..),
    prepareEnv,
    modifyEnv,
  )
where

import Conduit hiding (Producer)
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Unlift
import Data.Binary (Binary)
import Data.Conduit.Extras (conduitToCallbacks, sourceChan)
import Data.Conduit.Serialization.Binary
  ( conduitDecode,
    conduitEncode,
  )
import Data.Map qualified as M
import GHC.IO.Exception
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.Agent.NixPath (renderNixPath)
import Paths_hercules_ci_agent (getBinDir)
import Protolude
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error
import System.Process
import System.Timeout (timeout)
import Prelude ()

data WorkerException = WorkerException
  { originalException :: SomeException,
    exitStatus :: Maybe ExitCode
  }
  deriving (Show, Typeable)

instance Exception WorkerException where
  displayException we =
    displayException (originalException we)
      <> case exitStatus we of
        Nothing -> ""
        Just s -> " (worker: " <> show s <> ")"

data WorkerEnvSettings = WorkerEnvSettings
  { nixPath :: [EvaluateTask.NixPathElement (EvaluateTask.SubPathOf FilePath)],
    extraEnv :: Map [Char] [Char]
  }

-- | Filter out impure env vars by wildcard, set NIX_PATH
modifyEnv :: WorkerEnvSettings -> Map [Char] [Char] -> Map [Char] [Char]
modifyEnv workerEnvSettings =
  M.union (extraEnv workerEnvSettings)
    . M.insert "NIX_PATH" (toS $ renderNixPath $ nixPath workerEnvSettings)
    . M.filterWithKey (\k _v -> not ("NIXPKGS_" `isPrefixOf` k))
    . M.filterWithKey (\k _v -> not ("NIXOS_" `isPrefixOf` k))
    . M.delete "IN_NIX_SHELL"

prepareEnv :: WorkerEnvSettings -> IO [([Char], [Char])]
prepareEnv workerEnvSettings = do
  envMap <- M.fromList <$> getEnvironment
  pure $ M.toList $ modifyEnv workerEnvSettings envMap

getWorkerExe :: (MonadIO m) => m [Char]
getWorkerExe = do
  liftIO getBinDir <&> (</> "hercules-ci-agent-worker")

getDaemonExe :: (MonadIO m) => m [Char]
getDaemonExe = do
  liftIO getBinDir <&> (</> "hercules-ci-nix-daemon")

-- | Control a child process by communicating over stdin and stdout
-- using a 'Binary' interface.
runWorker ::
  (Binary command, Binary event, MonadUnliftIO m, MonadThrow m) =>
  -- | Process invocation details. Will ignore std_in, std_out and std_err fields.
  CreateProcess ->
  (Int -> ByteString -> m ()) ->
  Chan (Maybe command) ->
  (event -> m ()) ->
  m ExitCode
runWorker baseProcess stderrLineHandler commandChan eventHandler = do
  UnliftIO {unliftIO = unlift} <- askUnliftIO
  let createProcessSpec =
        baseProcess
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
          }
  liftIO $
    withCreateProcess createProcessSpec $ \mIn mOut mErr processHandle -> do
      (inHandle, outHandle, errHandle) <-
        case (,,) <$> mIn <*> mOut <*> mErr of
          Just x -> pure x
          Nothing ->
            throwIO $
              mkIOError
                illegalOperationErrorType
                "Process did not return all handles"
                Nothing -- no handle
                Nothing -- no path
      pidMaybe <- liftIO $ getPid processHandle
      let pid = maybe 0 fromIntegral pidMaybe
      let stderrPiper =
            liftIO $
              runConduit
                (sourceHandle errHandle .| linesUnboundedAsciiC .| awaitForever (liftIO . unlift . stderrLineHandler pid))
      let eventConduit = sourceHandle outHandle .| conduitDecode
          commandConduit =
            sourceChan commandChan
              .| conduitEncode
              .| concatMapC (\x -> [Chunk x, Flush])
              .| handleC handleEPIPE (sinkHandleFlush inHandle)
          handleEPIPE e | ioeGetErrorType e == ResourceVanished = pure ()
          handleEPIPE e = throwIO e
      let cmdThread = runConduit commandConduit `finally` hClose outHandle
          eventThread = unlift $ conduitToCallbacks eventConduit eventHandler
      -- plain forkIO so it can process all of stderr in case of an exception
      void $ forkIO stderrPiper
      withAsync (waitForProcess processHandle) $ \exitAsync -> do
        withAsync cmdThread $ \_ -> do
          eventThread
            `Safe.catch` \e -> do
              let oneSecond = 1000 * 1000
              maybeStatus <- timeout (5 * oneSecond) (wait exitAsync)
              throwIO $ WorkerException e maybeStatus
          wait exitAsync
