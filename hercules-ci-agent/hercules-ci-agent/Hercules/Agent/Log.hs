{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.Agent.Log
  ( module Katip,
    logLocM,
    module Hercules.Agent.Log,
    getLoc,
  )
where

import Data.Aeson
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as M
import Data.Vector (Vector)
import Hercules.API.Logs.LogEntry (LogEntry)
import Hercules.API.Logs.LogEntry qualified as LogEntry
import Katip hiding (logLocM)
import Katip.Core
import Katip.Monadic (logLocM)
import Protolude

withNamedContext :: (ToJSON a, KatipContext m) => Text -> a -> m b -> m b
withNamedContext name = katipAddContext . Katip.sl name

withContext :: (KatipContext m, LogItem a) => a -> m b -> m b
withContext = katipAddContext

-- TODO: Support context for all exceptions and use plain @panic@ instead.
panicWithLog :: KatipContext m => Text -> m a
panicWithLog msg = do
  logLocM ErrorS $ logStr msg
  panic msg

makeStderrLogItem :: ByteString -> LogEntry
makeStderrLogItem msg = LogEntry.Msg {msg = decodeUtf8With lenientDecode msg, i = 0, ms = 0, level = 0}

stderrLineHandler :: KatipContext m => (Vector LogEntry -> IO ()) -> Map Text Value -> Text -> Int -> ByteString -> m ()
stderrLineHandler _sendLogItems callerContext _processRole _ ln
  | "@katip " `BS.isPrefixOf` ln,
    Just item <- A.decode (LBS.fromStrict $ BS.drop 7 ln) =
      -- "This is the lowest level function [...] useful when implementing centralised logging services."
      Katip.Core.logKatipItem (Katip.Core.SimpleLogPayload . M.toList . fmap (Katip.Core.AnyLogPayload :: A.Value -> Katip.Core.AnyLogPayload) . extendContext <$> item)
  where
    extendContext workerItem = M.union workerItem callerContext
stderrLineHandler sendLogItems _ _processRole _pid ln =
  liftIO $ sendLogItems $ pure $ makeStderrLogItem ln
