{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.AttributeError where

import Data.Binary
import Protolude
import Prelude ()

data AttributeError
  = AttributeError
      { path :: [ByteString],
        message :: Text,
        errorType :: Maybe Text,
        errorDerivation :: Maybe Text
        }
  deriving (Generic, Binary, Show, Eq)
