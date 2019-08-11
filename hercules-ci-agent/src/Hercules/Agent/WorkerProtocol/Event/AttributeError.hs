{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Event.AttributeError where

import           Prelude                        ( )
import           Protolude
import           Data.Binary

data AttributeError = AttributeError
  { path :: [ByteString]
  , message :: Text
  , errorType :: Maybe Text
  , errorDerivation :: Maybe Text
  }
  deriving (Generic, Binary, Show, Eq)
