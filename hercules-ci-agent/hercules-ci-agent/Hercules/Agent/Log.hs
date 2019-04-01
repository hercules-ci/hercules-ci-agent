{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hercules.Agent.Log
  ( module Katip
  , logLocM
  , module Hercules.Agent.Log
  , getLoc
  )
where

import           Protolude

import           Data.Aeson
import           Katip hiding (logLocM)
import           Katip.Core
import           Katip.Monadic (logLocM)

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
