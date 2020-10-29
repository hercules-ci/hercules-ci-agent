{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.Attribute where

import Data.Binary
import Protolude
import Prelude ()

data AttributeType
  = Regular
  | MustFail
  | MayFail
  | DependenciesOnly
  | Effect
  deriving (Generic, Binary, Show, Eq)

data Attribute = Attribute
  { path :: [ByteString],
    drv :: ByteString,
    typ :: AttributeType
  }
  deriving (Generic, Binary, Show, Eq)
