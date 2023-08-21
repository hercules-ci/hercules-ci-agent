{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Hercules.Agent.WorkerProtocol.WorkerConfig where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Hercules.CNix.Verbosity qualified
import Katip qualified
import Protolude

-- | Sets up the worker environment.
data WorkerConfig = WorkerConfig
  { -- | Verbosity
    verbosity :: !(ViaShowRead Katip.Severity),
    -- | Nix Verbosity
    nixVerbosity :: !(ViaShowRead Hercules.CNix.Verbosity.Verbosity),
    -- | Nix Options
    nixOptions :: ![(Text, Text)]
  }
  deriving (Generic, Show)
  -- This uses JSON so that we can getLine and parse it in the worker before
  -- doing any command parsing. Maybe we don't need framing (a line) and we
  -- can switch to Binary?
  deriving anyclass (ToJSON, FromJSON)

newtype ViaShowRead a = ViaShowRead {unViaShowRead :: a}
  deriving newtype (Generic, Show, Read, Eq, Ord)

instance (Show a) => ToJSON (ViaShowRead a) where
  toJSON = toJSON @[Char] . show . unViaShowRead

instance (Read a) => FromJSON (ViaShowRead a) where
  parseJSON v = do
    s <- parseJSON @[Char] v
    case readMaybe s of
      Nothing -> fail $ "Could not parse " <> show s
      Just a -> pure $ ViaShowRead a
