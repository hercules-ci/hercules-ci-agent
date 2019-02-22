{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Command.Build where

import           Protolude
import           Data.Binary

data Build = Build
          { drvPath :: Text
          }
          deriving (Generic, Binary, Show, Eq)
