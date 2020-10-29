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
import qualified Data.Map as M
import Katip hiding (logLocM)
import Katip.Core
import Katip.Monadic (logLocM)
import Protolude

instance StringConv [Char] LogStr where
  strConv l = logStr . (strConv l :: [Char] -> Text)

instance StringConv Text LogStr where
  strConv _ = logStr

withNamedContext :: (ToJSON a, KatipContext m) => Text -> a -> m b -> m b
withNamedContext name = katipAddContext . Katip.sl name

withContext :: (KatipContext m, LogItem a) => a -> m b -> m b
withContext = katipAddContext

-- TODO: Support context for all exceptions and use plain @panic@ instead.
panicWithLog :: KatipContext m => Text -> m a
panicWithLog msg = do
  logLocM ErrorS $ toSL msg
  panic msg

stderrLineHandler :: KatipContext m => Text -> Int -> ByteString -> m ()
stderrLineHandler _processRole _ ln
  | "@katip " `BS.isPrefixOf` ln,
    Just item <- A.decode (toS $ BS.drop 7 ln) =
    -- "This is the lowest level function [...] useful when implementing centralised logging services."
    Katip.Core.logKatipItem (Katip.Core.SimpleLogPayload . M.toList . fmap (Katip.Core.AnyLogPayload :: A.Value -> Katip.Core.AnyLogPayload) <$> item)
stderrLineHandler processRole pid ln =
  withNamedContext "worker" (pid :: Int) $
    logLocM InfoS $
      logStr $
        processRole <> ": " <> toSL ln
