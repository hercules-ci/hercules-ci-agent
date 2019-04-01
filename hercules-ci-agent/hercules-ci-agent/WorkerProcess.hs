module WorkerProcess
  ( runWorker
  )
where

import           Prelude                        ( )
import           Protolude
import           System.Process
import           System.IO.Error
import           Conduit
import           Data.Conduit.Serialization.Binary
                                                ( conduitEncode
                                                , conduitDecode
                                                )
import           Data.Binary                    ( Binary )

-- | Control a child process by communicating over stdin and stdout
-- using a 'Binary' interface.
runWorker :: (Binary command, Binary event)
          => CreateProcess -- ^ Process invocation details. Will ignore std_in, std_out and std_err fields.
          -> ConduitM ByteString Void IO ()
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

    let stderrPiper = runConduit
          (sourceHandle errHandle .| linesUnboundedAsciiC .| stderrSink)

    let eventSource = sourceHandle outHandle .| conduitDecode
        commandSink =
          conduitEncode
            .| concatMapC (\x -> [Chunk x, Flush])
            .| sinkHandleFlush inHandle

    let interactor = runConduit $
          interaction eventSource `fuseUpstream` commandSink

    r <- runConcurrently (Concurrently stderrPiper *> Concurrently interactor)
    e <- waitForProcess processHandle
    pure (e, r)
