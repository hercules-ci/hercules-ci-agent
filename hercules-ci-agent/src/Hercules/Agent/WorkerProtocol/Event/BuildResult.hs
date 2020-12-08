{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.BuildResult where

import Data.Binary
import Protolude

data BuildResult
  = BuildFailure {errorMessage :: Text}
  | BuildSuccess {outputs :: [OutputInfo]}
  deriving (Generic, Binary, Show, Eq)

data OutputInfo = OutputInfo
  { -- | e.g. out, dev
    name :: ByteString,
    -- | store path
    path :: ByteString,
    -- | typically sha256:...
    hash :: ByteString,
    -- | nar size in bytes
    size :: Int64
  }
  deriving (Generic, Binary, Show, Eq)
