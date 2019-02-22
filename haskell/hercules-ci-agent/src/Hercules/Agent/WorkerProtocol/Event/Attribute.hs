{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Event.Attribute where

import           Prelude                        ( )
import           Protolude
import           Data.Binary

data Attribute = Attribute
  { path :: [ByteString]
  , drv :: ByteString
  -- TODO: metadata
  }
  deriving (Generic, Binary, Show, Eq)
