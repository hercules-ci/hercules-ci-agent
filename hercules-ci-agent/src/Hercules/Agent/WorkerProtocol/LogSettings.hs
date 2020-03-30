{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.LogSettings where

import Data.Binary
import Protolude
import Text.Show

data LogSettings
  = LogSettings
      { path :: Text,
        host :: Text,
        token :: Sensitive Text
      }
  deriving (Generic, Binary, Show, Eq)

-- | newtype wrapper to avoid leaking sensitive data through Show
newtype Sensitive a = Sensitive {reveal :: a}
  deriving (Generic, Binary, Eq, Ord)

-- | @const "<sensitive>"@
instance Show (Sensitive a) where
  show _ = "<sensitive>"
