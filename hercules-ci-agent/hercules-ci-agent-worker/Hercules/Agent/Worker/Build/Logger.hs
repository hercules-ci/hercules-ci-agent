{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.Build.Logger where

import CNix.Internal.Context
import Conduit (filterC)
import Data.ByteString.Unsafe (unsafePackMallocCString)
import Data.Conduit (ConduitT, Flush (..), await, awaitForever, yield)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign (alloca, nullPtr, peek)
import Hercules.API.Logs.LogEntry (LogEntry)
import qualified Hercules.API.Logs.LogEntry as LogEntry
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
import System.Timeout (timeout)

C.context context

C.include "<cstring>"

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/globals.hh>"

C.include "aliases.h"

C.include "hercules-logger.hh"

C.using "namespace nix"

initLogger :: IO ()
initLogger =
  [C.throwBlock| void {
    herculesLogger = new HerculesLogger();
    nix::logger = herculesLogger;
  }|]

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

forNonNull_ :: Ptr a -> (Ptr a -> IO ()) -> IO ()
forNonNull_ p f = if p == nullPtr then pass else f p

forNonNull :: Ptr a -> (Ptr a -> IO b) -> IO (Maybe b)
forNonNull p f = if p == nullPtr then pure Nothing else Just <$> f p

-- popping multiple lines into an array would be nice
convertEntry :: Ptr HerculesLoggerEntry -> IO (LogEntry)
convertEntry logEntryPtr = alloca \millisPtr -> alloca \textStrPtr -> alloca \levelPtr -> alloca \activityIdPtr -> alloca \typePtr -> alloca \parentPtr -> alloca \fieldsPtrPtr ->
  do
    r <-
      [C.throwBlock| int {
        const HerculesLogger::LogEntry &ln = *$(HerculesLoggerEntry *logEntryPtr);
        *$(uint64_t *millisPtr) = ln.ms;
        switch (ln.entryType) {
          case 1:
            *$(const char **textStrPtr) = strdup(ln.text.c_str());
            *$(int *levelPtr) = ln.level;
            return ln.entryType;
          case 2:
            *$(const char **textStrPtr) = strdup(ln.text.c_str());
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
    ms_ <- peek millisPtr
    let i_ = 0
    case r of
      1 -> do
        textStr <- peek textStrPtr
        text_ <- unsafePackMallocCString textStr
        level_ <- peek levelPtr
        pure $ LogEntry.Msg
          { i = i_,
            ms = ms_,
            level = fromIntegral level_,
            msg = toSL text_
          }
      2 -> do
        text_ <- unsafePackMallocCString =<< peek textStrPtr
        act_ <- peek activityIdPtr
        level_ <- peek levelPtr
        parent_ <- peek parentPtr
        typ_ <- peek typePtr
        fields_ <- convertAndDeleteFields =<< peek fieldsPtrPtr
        pure $ LogEntry.Start
          { i = i_,
            ms = ms_,
            act = LogEntry.ActivityId act_,
            level = fromIntegral level_,
            typ = LogEntry.ActivityType typ_,
            text = toSL text_,
            parent = LogEntry.ActivityId parent_,
            fields = fields_
          }
      3 -> do
        act_ <- peek activityIdPtr
        pure $ LogEntry.Stop
          { i = i_,
            ms = ms_,
            act = LogEntry.ActivityId act_
          }
      4 -> do
        act_ <- peek activityIdPtr
        typ_ <- peek typePtr
        fields_ <- convertAndDeleteFields =<< peek fieldsPtrPtr
        pure $ LogEntry.Result
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
  ( [C.block| void { delete $(LoggerFields *fieldsPtr); }|]
  )
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
            *$(const char **stringPtr) = strdup(field.s.c_str());
            return 1;
          default:
            return -1;
        }
      }|]
                >>= \case
                  0 -> LogEntry.Int <$> peek uintPtr
                  1 -> LogEntry.String . toSL <$> unsafeMallocBS (peek stringPtr)
                  _ -> panic "convertAndDeleteFields invalid internal type"

close :: IO ()
close =
  [C.throwBlock| void {
    herculesLogger->close();
  }|]

--
-- Conduits for logger
--

withLoggerConduit :: MonadIO m => (ConduitT () (Vector LogEntry) m () -> IO ()) -> IO a -> IO a
withLoggerConduit logger io = withAsync (logger popper) $ \popperAsync ->
  ((io `finally` close) <* wait popperAsync) `onException` timeout 2_000_000 (wait popperAsync)
  where
    popper = liftIO popMany >>= \case
      lns | null lns -> pass
      lns -> do
        yield lns
        popper

-- | Remove spammy progress results. Use 'nubProgress' instead?
filterProgress :: ConduitT (Flush LogEntry) (Flush LogEntry) IO ()
filterProgress = filterC \case
  Chunk (LogEntry.Result {rtype = LogEntry.ResultTypeProgress}) -> False
  _ -> True

nubProgress :: Monad m => ConduitT (Flush LogEntry) (Flush LogEntry) m ()
nubProgress = nubSubset (toChunk >=> toProgressKey)
  where
    toProgressKey k@(LogEntry.Result {rtype = LogEntry.ResultTypeProgress}) = Just k {LogEntry.i = 0}
    toProgressKey _ = Nothing
    toChunk (Chunk a) = Just a
    toChunk Flush = Nothing

unbatch :: (Monad m, Foldable l) => ConduitT (l a) (Flush a) m ()
unbatch = awaitForever $ \l -> do
  for_ l $ \a -> yield $ Chunk a
  yield Flush

batch :: Monad m => ConduitT (Flush a) [a] m ()
batch = go []
  where
    go acc = await >>= \case
      Nothing -> do
        unless (null acc) (yield $ reverse acc)
      Just Flush -> do
        unless (null acc) (yield $ reverse acc)
        go []
      Just (Chunk c) -> do
        go (c : acc)

nubSubset :: (Eq k, Monad m) => (a -> Maybe k) -> ConduitT a a m ()
nubSubset toKey = await >>= \case
  Nothing -> pass
  Just firstA -> yield firstA
    >> case toKey firstA of
      Nothing -> nubSubset toKey
      Just firstK -> nubSubset1 toKey firstK

nubSubset1 :: (Eq k, Monad m) => (a -> Maybe k) -> k -> ConduitT a a m ()
nubSubset1 toKey prevKey = await >>= \case
  Nothing -> pass
  Just a -> case toKey a of
    Nothing -> do
      yield a
      nubSubset1 toKey prevKey
    Just ak -> do
      unless (ak == prevKey) do
        yield a
      nubSubset1 toKey ak
