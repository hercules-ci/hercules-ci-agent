module Hercules.Agent.WorkerProcess
  ( runWorker
    )
where

import Conduit hiding (Producer)
import qualified Control.Exception.Safe as Safe
import Control.Monad.IO.Unlift
import Data.Binary (Binary)
import Data.Conduit.Extras (conduitToCallbacks, sourceChan)
import Data.Conduit.Serialization.Binary
  ( conduitDecode,
    conduitEncode
    )
import GHC.IO.Exception
import Protolude
import System.IO (hClose)
import System.IO.Error
import System.Process
import System.Timeout (timeout)
import Prelude ()

data WorkerException
  = WorkerException
      { originalException :: SomeException,
        exitStatus :: Maybe ExitCode
        }
  deriving (Show, Typeable)

instance Exception WorkerException

-- | Control a child process by communicating over stdin and stdout
-- using a 'Binary' interface.
runWorker
  :: (Binary command, Binary event, MonadUnliftIO m, MonadThrow m)
  => CreateProcess -- ^ Process invocation details. Will ignore std_in, std_out and std_err fields.
  -> (Int -> ByteString -> m ())
  -> Chan (Maybe command)
  -> (event -> m ())
  -> m ExitCode
runWorker baseProcess stderrLineHandler commandChan eventHandler = do
  UnliftIO {unliftIO = unlift} <- askUnliftIO
  let createProcessSpec =
        baseProcess
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
            }
  liftIO $ withCreateProcess createProcessSpec $ \mIn mOut mErr processHandle -> do
    (inHandle, outHandle, errHandle) <-
      case (,,) <$> mIn <*> mOut <*> mErr of
        Just x -> pure x
        Nothing ->
          throwIO
            $ mkIOError illegalOperationErrorType
                "Process did not return all handles"
                Nothing -- no handle
                Nothing -- no path
    pidMaybe <- liftIO $ getPid processHandle
    let pid = case pidMaybe of { Just x -> fromIntegral x; Nothing -> 0 }
    let stderrPiper =
          liftIO
            $ runConduit
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
