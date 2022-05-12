{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.AttributeIFD where

import Data.Binary (Binary)
import Protolude
import Prelude ()

data AttributeIFD = AttributeIFD
  { path :: [ByteString],
    derivationPath :: ByteString,
    derivationOutput :: ByteString,
    done :: Bool
  }
  deriving (Generic, Binary, Show, Eq)
