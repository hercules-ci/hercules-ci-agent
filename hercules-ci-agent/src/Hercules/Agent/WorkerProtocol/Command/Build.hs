{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Build where

import Data.Binary
import Protolude
import Text.Show

data Build
  = Build
      { drvPath :: Text,
        inputDerivationOutputPaths :: [ByteString],
        logToken :: Sensitive Text
      }
  deriving (Generic, Binary, Show, Eq)

-- | newtype wrapper to avoid leaking sensitive data through Show
newtype Sensitive a = Sensitive a
  deriving (Generic, Binary, Eq, Ord)

-- | @const "<sensitive>"@
instance Show (Sensitive a) where
  show _ = "<sensitive>"
