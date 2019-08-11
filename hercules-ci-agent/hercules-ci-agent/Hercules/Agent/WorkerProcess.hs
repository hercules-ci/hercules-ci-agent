module Hercules.Agent.WorkerProcess
  ( runWorker
  )
where

import           Prelude                        ( )
import           Protolude
import           System.Process
import           System.IO.Error
import           Conduit
import qualified Control.Exception.Safe        as Safe
import           Data.Conduit.Serialization.Binary
                                                ( conduitEncode
                                                , conduitDecode
                                                )
import           Data.Binary                    ( Binary )
import           System.Timeout                 ( timeout )
import           GHC.IO.Exception

data WorkerException = WorkerException
  { originalException :: SomeException
  , exitStatus :: Maybe ExitCode
  } deriving (Show, Typeable)
instance Exception WorkerException

-- | Control a child process by communicating over stdin and stdout
-- using a 'Binary' interface.
runWorker :: (Binary command, Binary event)
          => CreateProcess -- ^ Process invocation details. Will ignore std_in, std_out and std_err fields.
          -> (Int -> ConduitM ByteString Void IO ())
          -> ( forall i
              . ConduitM i event IO ()
             -> ConduitM i command IO a
             )
          -> IO (ExitCode, a)
runWorker baseProcess stderrSink interaction = do
  let createProcessSpec = baseProcess { std_in = CreatePipe
                                      , std_out = CreatePipe
                                      , std_err = CreatePipe
                                      }


  withCreateProcess createProcessSpec $ \mIn mOut mErr processHandle -> do
    (inHandle, outHandle, errHandle) <- case (,,) <$> mIn <*> mOut <*> mErr of
      Just x -> pure x
      Nothing -> throwIO $ mkIOError illegalOperationErrorType
                                     "Process did not return all handles"
                                     Nothing -- no handle
                                     Nothing -- no path

    pidMaybe <- getPid processHandle
    let pid = case pidMaybe of Just x -> fromIntegral x; Nothing -> 0

    let stderrPiper = runConduit
          (sourceHandle errHandle .| linesUnboundedAsciiC .| stderrSink pid)

    let eventSource = sourceHandle outHandle .| conduitDecode
        handleEPIPE e | ioeGetErrorType e == ResourceVanished = pure ()
        handleEPIPE e = throwIO e
        commandSink =
          conduitEncode
            .| concatMapC (\x -> [Chunk x, Flush])
            .| handleC handleEPIPE (sinkHandleFlush inHandle)

    let interactor =
          runConduit $ interaction eventSource `fuseUpstream` commandSink

    withAsync (waitForProcess processHandle) $ \exitAsync -> do
      r <-
       runConcurrently (Concurrently stderrPiper *> Concurrently interactor)
        `Safe.catch` \e -> do
          let oneSecond = 1000 * 1000
          maybeStatus <- timeout (5 * oneSecond) (wait exitAsync)
          throwIO $ WorkerException e maybeStatus
      e <- wait exitAsync
      pure (e, r)
