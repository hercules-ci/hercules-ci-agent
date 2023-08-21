{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.OutputInfo where

import Data.Binary
import Protolude

data OutputInfo = OutputInfo
  { -- | e.g. out, dev
    name :: ByteString,
    -- | store path
    path :: ByteString,
    -- | typically sha256:...
    hash :: ByteString,
    -- | nar size in bytes
    size :: Int64,
    -- | references, in store path basename format
    references :: [ByteString]
  }
  deriving (Generic, Binary, Show, Eq)
