{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.Attribute where

import Data.Binary
import Protolude
import Prelude ()

data Attribute
  = Attribute
      { path :: [ByteString],
        drv :: ByteString
        -- TODO: metadata
        }
  deriving (Generic, Binary, Show, Eq)
