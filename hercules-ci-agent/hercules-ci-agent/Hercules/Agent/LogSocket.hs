{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Hercules.Agent.LogSocket where

import Conduit
  ( ConduitM,
    ConduitT,
    Flush (..),
    await,
    awaitForever,
    filterC,
    fuseUpstream,
    mapC,
    runConduit,
    yield,
    (.|),
  )
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Conc (labelThread)
import Hercules.API.Agent.LifeCycle.ServiceInfo qualified
import Hercules.API.Logs.LogEntry (LogEntry ())
import Hercules.API.Logs.LogEntry qualified as LogEntry
import Hercules.API.Logs.LogHello (LogHello (LogHello))
import Hercules.API.Logs.LogHello qualified
import Hercules.API.Logs.LogMessage (LogMessage)
import Hercules.API.Logs.LogMessage qualified as LogMessage
import Hercules.Agent.Conduit (takeCWhileStopEarly, withMessageLimit)
import Hercules.Agent.Log (KatipContext, Severity (DebugS, ErrorS), katipAddContext, logLocM, sl)
import Hercules.Agent.Socket qualified as Socket
import Hercules.CNix.Store (getClientProtocolVersion)
import Katip (KatipContext (getKatipContext, getKatipNamespace), KatipContextT, Severity (WarningS), getLogEnv, runKatipContextT)
import Network.URI qualified
import Protolude hiding (atomically, finally, forkFinally, myThreadId, withAsync, yield)
import System.Timeout.Lifted (timeout)
import UnliftIO (MonadUnliftIO, atomically, catch, finally, lengthTBQueue, newTBQueueIO, readTBQueue, withAsync, writeTBQueue)
import UnliftIO.Concurrent (forkFinally, myThreadId)

data LogSettings = LogSettings
  { path :: !Text,
    baseURL :: !Text,
    token :: !Text
  }
  deriving (Generic)

withLoggerNoFlush ::
  (MonadUnliftIO m, KatipContext m) =>
  -- | Label for the logger thread
  Text ->
  -- | Store protocol version
  Int ->
  LogSettings ->
  -- | The logger function. It immediately replaces the time field of the log entries and queues them for asynchronous delivery.
  ((Vector LogEntry -> IO ()) -> m a) ->
  m a
withLoggerNoFlush label storeProtocolVersion settings f = do
  start <- liftIO getCurrentTime
  (loggr, stop) <- forkLogger label storeProtocolVersion settings
  f (loggr <=< setLogEntriesTime start) `finally` liftIO stop

setLogEntriesTime :: (Functor l) => UTCTime -> l LogEntry -> IO (l LogEntry)
setLogEntriesTime start l = do
  now <- getCurrentTime
  let diffMs = floor $ 1000 * diffUTCTime now start
  pure $ l <&> \e -> e {LogEntry.ms = diffMs}

forkLogger ::
  forall m.
  (MonadUnliftIO m, KatipContext m) =>
  -- | Label for the logger thread
  Text ->
  -- | Store protocol version
  Int ->
  LogSettings ->
  m (Vector LogEntry -> IO (), IO ())
forkLogger label storeProtocolVersion settings = katipAddContext (sl "label" label) do
  q <- newTBQueueIO 100000

  let entriesSource :: ConduitM () (Vector LogEntry) m ()
      entriesSource = do
        item <- lift do
          ( atomically do
              entries <- readTBQueue q
              pure entries
            )
            `UnliftIO.catch` ( \(e :: SomeException) -> do
                                 katipAddContext (sl "exception" (show e :: Text)) do
                                   logLocM ErrorS $ "exception in entriesSource"
                                   throwIO e
                             )
        case item of
          Nothing -> pass -- terminate conduit
          Just entries -> yield entries >> entriesSource

  _threadId <- flip
    forkFinally
    ( \case
        Right () -> logLocM DebugS "Logger done"
        Left e -> katipAddContext (sl "exception" (show e :: Text)) do
          logLocM ErrorS "Logger failed"
    )
    do
      t <- myThreadId
      liftIO (labelThread t ("logger for " <> toS label))
      logger settings storeProtocolVersion entriesSource
      pass
  (UnliftKatipContextT unliftKatip) <- askUnliftKatipContextT
  let putQ item = do
        l <- atomically do
          writeTBQueue q item
          lengthTBQueue q
        when (l > 10000) do
          unliftKatip $ logLocM WarningS "Logger queue has significant backlog."
  pure
    ( putQ . Just,
      putQ Nothing
    )

logger :: (MonadUnliftIO m, KatipContext m) => LogSettings -> Int -> ConduitM () (Vector LogEntry) m () -> m ()
logger logSettings_ storeProtocolVersionValue entriesSource = do
  socketConfig <- liftIO $ makeSocketConfig logSettings_ storeProtocolVersionValue
  let withPings socket m =
        withAsync
          ( liftIO $ forever do
              -- TODO add ping constructor to Frame or use websocket pings
              let ping = LogMessage.LogEntries mempty
              threadDelay 30_000_000
              atomically $ Socket.write socket ping
          )
          (const m)
  Socket.withReliableSocket socketConfig $ \socket -> withPings socket do
    let conduit =
          entriesSource
            .| unbatch
            .| filterProgress
            .| dropMiddle
            .| renumber 0
            .| batchAndEnd
            .| socketSink socket
        batch' = batch .| mapC (LogMessage.LogEntries . V.fromList)
        batchAndEnd =
          (foldMapTap (Last . ims) `fuseUpstream` batch') >>= \case
            Last (Just (i, ms)) -> yield $ LogMessage.End {i = i + 1, ms = ms}
            Last Nothing -> yield $ LogMessage.End 0 0
          where
            ims (Chunk logEntry) = Just (logEntry.i, logEntry.ms)
            ims _ = Nothing
        renumber i =
          await >>= traverse_ \case
            Flush -> yield Flush >> renumber i
            Chunk e -> do
              yield $ Chunk e {LogEntry.i = i}
              renumber (i + 1)
    runConduit conduit
    logLocM DebugS "Syncing"
    liftIO (timeout 600_000_000 $ Socket.syncIO socket) >>= \case
      Just _ -> pass
      Nothing -> panic "Could not push logs within 10 minutes after completion"
    logLocM DebugS "Logger done"

makeSocketConfig :: (MonadIO m) => LogSettings -> Int -> IO (Socket.SocketConfig LogMessage Hercules.API.Agent.LifeCycle.ServiceInfo.ServiceInfo m)
makeSocketConfig l storeProtocolVersionValue = do
  clientProtocolVersionValue <- liftIO getClientProtocolVersion
  baseURL_ <- case Network.URI.parseURI $ toS l.baseURL of
    Just x -> pure x
    Nothing -> panic "LogSettings: invalid base url"
  pure
    Socket.SocketConfig
      { makeHello =
          pure $
            LogMessage.Hello
              LogHello
                { clientProtocolVersion = clientProtocolVersionValue,
                  storeProtocolVersion = storeProtocolVersionValue
                },
        checkVersion = Socket.checkVersion',
        baseURL = baseURL_,
        path = l.path,
        token = encodeUtf8 l.token
      }

batch :: (Monad m) => ConduitT (Flush a) [a] m ()
batch = go []
  where
    go acc =
      await >>= \case
        Nothing -> do
          unless (null acc) (yield $ reverse acc)
        Just Flush -> do
          unless (null acc) (yield $ reverse acc)
          go []
        Just (Chunk c) -> do
          go (c : acc)

unbatch :: (Monad m, Foldable l) => ConduitT (l a) (Flush a) m ()
unbatch = awaitForever $ \l -> do
  for_ l $ \a -> yield $ Chunk a
  yield Flush

-- Sum must be < 100_000
richLogLimit, textOnlyLogLimit, tailLimit :: Int
richLogLimit = 40_000
textOnlyLogLimit = 49_900
tailLimit = 10_000

dropMiddle :: (MonadIO m) => ConduitM (Flush LogEntry) (Flush LogEntry) m ()
dropMiddle = do
  -- rich logging
  _ <- takeCWhileStopEarly isChunk richLogLimit
  -- degrade to text logging (in case rich logging produces excessive non-textual data)
  visibleLinesOnly .| withMessageLimit isChunk textOnlyLogLimit tailLimit snipStart snip snipped

snipStart :: (Monad m) => ConduitT (Flush LogEntry) (Flush LogEntry) m ()
snipStart =
  yield $
    Chunk $
      LogEntry.Msg
        { i = 0,
          ms = 0,
          level = 0 {- error -},
          msg = "hercules-ci-agent: Soft log limit has been reached. Final log lines will appear when done."
        }

snipped :: (Monad m) => Int -> ConduitT (Flush LogEntry) (Flush LogEntry) m ()
snipped n =
  yield $
    Chunk $
      LogEntry.Msg
        { i = 0,
          ms = 0,
          level = 0 {- error -},
          msg = "hercules-ci-agent: " <> show n <> " log lines were omitted before the last " <> show tailLimit <> "."
        }

snip :: (Monad m) => Int -> ConduitT (Flush LogEntry) (Flush LogEntry) m ()
snip n =
  yield $
    Chunk $
      LogEntry.Msg
        { i = 0,
          ms = 0,
          level = 0 {- error -},
          msg = "hercules-ci-agent: skipping " <> show n <> " log lines."
        }

visibleLinesOnly :: (Monad m) => ConduitM (Flush LogEntry) (Flush LogEntry) m ()
visibleLinesOnly =
  filterC isVisible

isVisible :: Flush LogEntry -> Bool
isVisible Flush = True
isVisible (Chunk LogEntry.Msg {msg = msg}) | msg /= "" = True
isVisible (Chunk LogEntry.Start {text = msg}) | msg /= "" = True
isVisible (Chunk LogEntry.Result {rtype = LogEntry.ResultTypeBuildLogLine}) = True
isVisible _ = False

isChunk :: Flush LogEntry -> Bool
isChunk Chunk {} = True
isChunk _ = False

socketSink :: (MonadIO m) => Socket.Socket r w -> ConduitT w o m ()
socketSink socket = awaitForever $ liftIO . atomically . Socket.write socket

-- | Perform a foldMap while yielding the original values ("tap").
--
-- '<>' is invoked with the new b on the right.
foldMapTap :: (Monoid b, Monad m) => (a -> b) -> ConduitT a a m b
foldMapTap f = go mempty
  where
    go b =
      await >>= \case
        Nothing -> pure b
        Just a -> do
          yield a
          go (b <> f a)

-- TODO: Use 'nubProgress' instead?

-- | Remove spammy progress results.
filterProgress :: (Monad m) => ConduitT (Flush LogEntry) (Flush LogEntry) m ()
filterProgress = filterC \case
  Chunk LogEntry.Result {rtype = LogEntry.ResultTypeProgress} -> False
  Chunk LogEntry.Result {rtype = LogEntry.ResultTypeSetExpected} -> False
  _ -> True

----- Utilities -----

-- | Captures the whole logging context, by providing the capability to reduce @KatipContextT m@ to a plain @m@.
newtype UnliftKatipContextT = UnliftKatipContextT {unliftKatipContextT :: forall a m. KatipContextT m a -> m a}

askUnliftKatipContextT :: (KatipContext m) => m UnliftKatipContextT
askUnliftKatipContextT = do
  le <- getLogEnv
  ctx <- getKatipContext
  ns <- getKatipNamespace
  pure (UnliftKatipContextT (runKatipContextT le ctx ns))
