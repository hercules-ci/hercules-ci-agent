{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Event.AttributeError where

import           Prelude                        ( )
import           Protolude
import           Data.Binary

data AttributeError = AttributeError
  { path :: [ByteString]
  , message :: Text
  }
  deriving (Generic, Binary, Show, Eq)
