module Hercules.CLI.Exception where

import qualified Control.Exception.Safe
import Hercules.UserException (UserException (UserException))
import Protolude hiding (handle, show)
import System.IO (hIsTerminalDevice)

handleUserException :: IO a -> IO a
handleUserException =
  Control.Exception.Safe.handle
    ( \(UserException msg) -> do
        stderrIsTerminal <- hIsTerminalDevice stderr
        if stderrIsTerminal
          then putErrText $ "hci: \ESC[31;1merror:\ESC[m " <> msg
          else putErrText $ "hci: error: " <> msg
        exitFailure
    )

exitMsg :: MonadIO m => Text -> m a
exitMsg = liftIO . throwIO . UserException
