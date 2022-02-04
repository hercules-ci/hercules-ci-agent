{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.NixDaemon where

import Control.Concurrent.STM.TVar (modifyTVar, newTVarIO, readTVar)
import qualified Data.Binary
import qualified Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.Agent.Binary (decodeBinaryFromHandle)
import qualified Hercules.Agent.WorkerProtocol.Command.StartDaemon as StartDaemon
import Hercules.Agent.WorkerProtocol.Event.DaemonStarted (DaemonStarted (DaemonStarted, _dummy))
import qualified Hercules.CNix as CNix
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Network.Socket
import Protolude
import System.Posix.Internals (setNonBlockingFD)
import UnliftIO (BufferMode (NoBuffering), hSetBuffering, timeout)
import UnliftIO.IO (hFlush)

C.context C.cppCtx

C.include "<nix/config.h>"
C.include "<nix/daemon.hh>"
C.include "<iostream>"
C.using "namespace nix"

nixDaemon :: IO ()
-- nixDaemon = withKatip $ Logger.withLoggerConduit (_) $ Logger.withTappedStderr Logger.tapper $ liftIO $ do
nixDaemon = do
  hSetBuffering stdin NoBuffering
  startCmd <-
    getStdinMessage >>= \case
      Left _ -> throwIO $ FatalError "Could not decode command"
      Right Nothing -> do
        putErrText "warning: Exit before receiving start command"
        exitSuccess -- We're done.
      Right (Just r) -> pure r
  let path = StartDaemon.socketPath startCmd

  CNix.init

  clientThreads <- newTVarIO mempty

  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix path)
  listen sock 100

  putStdoutMessage DaemonStarted {_dummy = True}

  let socketLoop =
        forever do
          (clientSocket, _) <- accept sock
          (pid, _uid, _gid) <- getPeerCredential clientSocket
          uninterruptibleMask \_unmask -> do
            let removeMe = do
                  t <- myThreadId
                  atomically do
                    modifyTVar clientThreads (M.delete t)
            t <-
              forkFinally -- TODO forkOS?
                (handleClient clientSocket)
                \case
                  Left e -> do
                    removeMe
                    putErrText ("Connection for pid " <> showPid pid <> " ended: " <> toS (displayException e))
                  Right () -> do
                    removeMe
                    putErrText ("Connection for pid " <> showPid pid <> " ended normally")
            atomically do
              modifyTVar clientThreads (M.insert t pid)
  withAsync socketLoop \_socketLoopAsync ->
    void getStdinMessage

  void $ timeout (10 * 60 * 1000 * 1000) do
    hadThreads <- do
      ts <- atomically do
        readTVar clientThreads
      let hasThreads = not (null ts)
      when hasThreads do
        putErrLn ("Waiting for termination of connections from pids " <> unwords (showPid <$> toList ts))
      pure hasThreads
    atomically do
      ts <- readTVar clientThreads
      guard $ null ts
    when hadThreads do
      putErrText "All connections terminated; exiting"

showPid :: Maybe C.CUInt -> Text
showPid = maybe "unknown pid" show

putStdoutMessage :: DaemonStarted -> IO ()
putStdoutMessage msg = do
  BS.hPut stdout $ BL.toStrict $ Data.Binary.encode msg
  hFlush stdout

getStdinMessage ::
  IO
    ( Either
        (ByteString, Data.Binary.Get.ByteOffset, [Char])
        (Maybe StartDaemon.StartDaemon)
    )
getStdinMessage = decodeBinaryFromHandle stdin

handleClient :: Socket -> IO ()
handleClient clientSocket = withFdSocket clientSocket \fd -> flip finally (close clientSocket) do
  setNonBlockingFD fd False
  [C.throwBlock| void {
    ref<Store> store = openStore();
    FdSource from($(int fd));
    FdSink to($(int fd));
    daemon::TrustedFlag trusted = daemon::NotTrusted;
    daemon::RecursiveFlag recursive = daemon::NotRecursive;
    std::function<void(Store &)> authHook = [](Store &){};
    daemon::processConnection(
        store
        , from
        , to
        , trusted
        , recursive
        , authHook
        );
  }|]
