module Hercules.CLI.Exception where

import qualified Control.Exception.Safe
import Protolude hiding (handle, show)
import System.IO (hIsTerminalDevice)
import Text.Show

data UserException = UserException Text

instance Exception UserException where
  displayException = show

instance Show UserException where
  show (UserException msg) = "error: " <> toS msg

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
