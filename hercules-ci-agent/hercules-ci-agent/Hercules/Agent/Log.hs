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
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
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

stderrLineHandler :: KatipContext m => Map Text Value -> Text -> Int -> ByteString -> m ()
stderrLineHandler callerContext _processRole _ ln
  | "@katip " `BS.isPrefixOf` ln,
    Just item <- A.decode (LBS.fromStrict $ BS.drop 7 ln) =
    -- "This is the lowest level function [...] useful when implementing centralised logging services."
    Katip.Core.logKatipItem (Katip.Core.SimpleLogPayload . M.toList . fmap (Katip.Core.AnyLogPayload :: A.Value -> Katip.Core.AnyLogPayload) . extendContext <$> item)
  where
    extendContext workerItem = M.union workerItem callerContext
stderrLineHandler _ processRole pid ln =
  withNamedContext "worker" (pid :: Int) $
    logLocM InfoS $
      logStr $
        processRole <> ": " <> decodeUtf8With lenientDecode ln
