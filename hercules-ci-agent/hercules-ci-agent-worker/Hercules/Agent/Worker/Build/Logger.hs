{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.Build.Logger
  ( initLogger,
    withLoggerConduit,
    nubProgress,
    plumbWorkerStd,
    Handles (..),
  )
where

import Conduit (MonadUnliftIO)
import Data.ByteString.Unsafe (unsafePackMallocCString)
import Data.Conduit (ConduitT, Flush (..), await, yield)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Foreign (alloca, nullPtr, peek)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hercules.API.Logs.LogEntry (LogEntry)
import Hercules.API.Logs.LogEntry qualified as LogEntry
import Hercules.Agent.Worker.Build.Logger.Context (Fields, HerculesLoggerEntry, context)
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Protolude hiding (bracket, finally, mask_, onException, tryJust, wait, withAsync, yield)
import System.IO (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import System.Timeout (timeout)
import UnliftIO.Async
import UnliftIO.Exception

C.context context

C.include "<cstring>"

#if NIX_IS_AT_LEAST(2, 28, 0)

-- C.include 

#else
C.include "<nix/config.h>"
C.include "<nix/shared.hh>"
C.include "<nix/globals.hh>"
#endif

C.include "<hercules-ci-cnix/string.hxx>"

C.include "hercules-aliases.h"

C.include "hercules-logger.hh"

C.using "namespace nix"

C.using "namespace hercules_ci_cnix"

initLogger :: IO ()
initLogger =
#if NIX_IS_AT_LEAST(2, 28, 0)
  [C.throwBlock| void {
    nix::logger = std::make_unique<HerculesLogger>();
    herculesLogger = dynamic_cast<HerculesLogger *>(nix::logger.get());
    if (herculesLogger == nullptr) {
      throw std::runtime_error("Failed to cast logger to HerculesLogger");
    }
  }|]
#else
  [C.throwBlock| void {
    herculesLogger = new HerculesLogger();
    nix::logger = herculesLogger;
  }|]
#endif

popMany :: IO (Vector LogEntry)
popMany =
  let limit = 100
   in bracket
        [C.exp| LogEntryQueue *{ new LogEntryQueue() }|]
        (\buf -> [C.block| void { delete $(LogEntryQueue *buf); }|])
        ( \buf -> do
            [C.block| void {
              herculesLogger->popMany($(int limit), *$(LogEntryQueue *buf));
            }|]
            let getBufHeadAndReinsertClose =
                  [C.block| HerculesLoggerEntry * {
                    LogEntryQueue &buf = *$(LogEntryQueue *buf);
                    if (buf.empty()) {
                      return nullptr;
                    } else {
                      auto r = buf.front().get();
                      if (r == nullptr) {
                        herculesLogger->close();
                      }
                      return r;
                    }
                  }|]
                dropBufHead =
                  [C.block| void { 
                    $(LogEntryQueue *buf)->pop();
                  }|]
                popBufHead = do
                  hdN <- getBufHeadAndReinsertClose
                  forNonNull hdN $ \hd -> do
                    r <- convertEntry hd
                    dropBufHead -- frees hd
                    pure r
            V.unfoldrM
              ( \_ -> do
                  (,()) <<$>> popBufHead
              )
              ()
        )

{-
popOne :: IO (Maybe LogEntry)
popOne = alloca \deleterPtr ->
  bracket [C.throwBlock| HerculesLoggerEntry * {
      auto linePtr = herculesLogger->pop();
      if (linePtr == nullptr) {
        return nullptr;
      } else {
        auto d = linePtr.get_deleter();
        // we need d in the closure for this to work, which is currently
        // hard to do
        *$(void (** deleterPtr)(HerculesLoggerEntry *)) =
          [d](HerculesLoggerEntry *v) { d(v); };
        return linePtr.release();
      }
    }|]
  (\entryNullable -> forNonNull_ entryNullable (\entry -> [C.block| void {
    (*$(void (** deleterPtr)(HerculesLoggerEntry *)))($(HerculesLoggerEntry *entry));
  }|]))
  (\entryNullable ->
    fmap join $ forNonNull entryNullable $ \entry ->
      convertEntry entry
  )
-}

forNonNull :: Ptr a -> (Ptr a -> IO b) -> IO (Maybe b)
forNonNull p f = if p == nullPtr then pure Nothing else Just <$> f p

-- popping multiple lines into an array would be nice
convertEntry ::
  Ptr HerculesLoggerEntry ->
  -- | 'LogEntry' without timestamp
  IO LogEntry
convertEntry logEntryPtr = alloca \textStrPtr -> alloca \levelPtr -> alloca \activityIdPtr -> alloca \typePtr -> alloca \parentPtr -> alloca \fieldsPtrPtr ->
  do
    r <-
      [C.throwBlock| int {
        const HerculesLogger::LogEntry &ln = *$(HerculesLoggerEntry *logEntryPtr);
        switch (ln.entryType) {
          case 1:
            *$(const char **textStrPtr) = stringdup(ln.text);
            *$(int *levelPtr) = ln.level;
            return ln.entryType;
          case 2:
            *$(const char **textStrPtr) = stringdup(ln.text);
            *$(int *levelPtr) = ln.level;
            *$(uint64_t *activityIdPtr) = ln.activityId;
            *$(uint64_t *typePtr) = ln.type;
            *$(uint64_t *parentPtr) = ln.parent;
            *$(LoggerFields **fieldsPtrPtr) = new nix::Logger::Fields(ln.fields);
            return ln.entryType;
          case 3:
            *$(uint64_t *activityIdPtr) = ln.activityId;
            return ln.entryType;
          case 4:
            *$(uint64_t *activityIdPtr) = ln.activityId;
            *$(uint64_t *typePtr) = ln.type;
            *$(LoggerFields **fieldsPtrPtr) = new nix::Logger::Fields(ln.fields);
            return ln.entryType;
          default:
            return 0;
        }
      }|]
    -- Real timestamps and index number are set by the log monitoring process instead
    let i_ = 0
        ms_ = 0
    case r of
      1 -> do
        textStr <- peek textStrPtr
        text_ <- unsafePackMallocCString textStr
        level_ <- peek levelPtr
        pure $
          LogEntry.Msg
            { i = i_,
              ms = ms_,
              level = fromIntegral level_,
              msg = decode text_
            }
      2 -> do
        text_ <- unsafePackMallocCString =<< peek textStrPtr
        act_ <- peek activityIdPtr
        level_ <- peek levelPtr
        parent_ <- peek parentPtr
        typ_ <- peek typePtr
        fields_ <- convertAndDeleteFields =<< peek fieldsPtrPtr
        pure $
          LogEntry.Start
            { i = i_,
              ms = ms_,
              act = LogEntry.ActivityId act_,
              level = fromIntegral level_,
              typ = LogEntry.ActivityType typ_,
              text = decode text_,
              parent = LogEntry.ActivityId parent_,
              fields = fields_
            }
      3 -> do
        act_ <- peek activityIdPtr
        pure $
          LogEntry.Stop
            { i = i_,
              ms = ms_,
              act = LogEntry.ActivityId act_
            }
      4 -> do
        act_ <- peek activityIdPtr
        typ_ <- peek typePtr
        fields_ <- convertAndDeleteFields =<< peek fieldsPtrPtr
        pure $
          LogEntry.Result
            { i = i_,
              ms = ms_,
              act = LogEntry.ActivityId act_,
              rtype = LogEntry.ResultType typ_,
              fields = fields_
            }
      _ -> panic "convertEntry invalid internal type"

convertAndDeleteFields :: Ptr Fields -> IO (Vector LogEntry.Field)
convertAndDeleteFields fieldsPtr = flip
  finally
  [C.block| void { delete $(LoggerFields *fieldsPtr); }|]
  do
    size <- [C.exp| size_t { $(LoggerFields *fieldsPtr)->size() }|]
    V.generateM (fromIntegral size) $ \i' ->
      mask_ $
        let i = fromIntegral i'
         in alloca \uintPtr -> alloca \stringPtr ->
              [C.block| int {
        const nix::Logger::Field &field = (*$(LoggerFields *fieldsPtr)).at($(int i));
        switch (field.type) {
          case nix::Logger::Field::tInt:
            *$(uint64_t *uintPtr) = field.i;
            return 0;
          case nix::Logger::Field::tString:
            *$(const char **stringPtr) = stringdup(field.s);
            return 1;
          default:
            return -1;
        }
      }|]
                >>= \case
                  0 -> LogEntry.Int <$> peek uintPtr
                  1 ->
                    LogEntry.String . decode
                      <$> (unsafePackMallocCString =<< peek stringPtr)
                  _ -> panic "convertAndDeleteFields invalid internal type"

decode :: ByteString -> Text
decode = decodeUtf8With lenientDecode

close :: IO ()
close =
  [C.throwBlock| void {
    herculesLogger->close();
  }|]

--
-- Conduits for logger
--

withLoggerConduit :: (MonadUnliftIO m) => (ConduitT () (Vector LogEntry) m () -> m ()) -> m a -> m a
withLoggerConduit logger io = withAsync (logger popper) $ \popperAsync ->
  ((io `finally` liftIO close) <* wait popperAsync) `onException` liftIO (timeout 2_000_000 (wait popperAsync))
  where
    popper =
      liftIO popMany >>= \case
        lns | null lns -> pass
        lns -> do
          yield lns
          popper

nubProgress :: (Monad m) => ConduitT (Flush LogEntry) (Flush LogEntry) m ()
nubProgress = nubSubset (toChunk >=> toProgressKey)
  where
    toProgressKey k@LogEntry.Result {rtype = LogEntry.ResultTypeProgress} = Just k {LogEntry.i = 0}
    toProgressKey _ = Nothing
    toChunk (Chunk a) = Just a
    toChunk Flush = Nothing

nubSubset :: (Eq k, Monad m) => (a -> Maybe k) -> ConduitT a a m ()
nubSubset toKey =
  await >>= \case
    Nothing -> pass
    Just firstA ->
      yield firstA
        >> case toKey firstA of
          Nothing -> nubSubset toKey
          Just firstK -> nubSubset1 toKey firstK

nubSubset1 :: (Eq k, Monad m) => (a -> Maybe k) -> k -> ConduitT a a m ()
nubSubset1 toKey prevKey =
  await >>= \case
    Nothing -> pass
    Just a -> case toKey a of
      Nothing -> do
        yield a
        nubSubset1 toKey prevKey
      Just ak -> do
        unless (ak == prevKey) do
          yield a
        nubSubset1 toKey ak

data Handles = Handles {commands :: Handle, events :: Handle}

-- | Reassign file descriptors according to the scheme for the worker process.
--
-- __Before__
--
-- These are the roles assigned by the parent process:
--
--     * stdin: commands
--
--     * stdout: events
--
--     * stderr: arbitrary logs
--
-- __After__
--
--  Code in the child process can assume a pretty normal environment:
--
--     * stdin: empty
--
--     * stdout: arbitrary logs
--
--     * stderr: arbitrary logs
--
-- __Return__
--
-- The special handles that let us do structured communication with the parent process:
--
--    * commands
--
--    * events
--
-- __Output Buffering__
--
-- The standard handles are set to line buffering. The events handle is set to no buffering.
--
-- __Note__
--
-- This function is not interruptible and not thread-safe.
plumbWorkerStd :: IO Handles
plumbWorkerStd =
  Protolude.uninterruptibleMask \_unmask -> do
    -- in
    commandsHandle <- hDuplicate stdin
    devNull <- openFile "/dev/null" ReadMode
    hDuplicateTo devNull stdin

    -- out
    eventsHandle <- hDuplicate stdout
    hDuplicateTo stderr stdout

    -- err: unchanged

    -- buffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    hSetBuffering eventsHandle NoBuffering

    pure Handles {commands = commandsHandle, events = eventsHandle}
