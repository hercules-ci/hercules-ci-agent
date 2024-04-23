{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Hercules.Agent.NixFile.HerculesCIArgs where

import Data.Aeson (ToJSON)
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.NixFile.GitSource qualified as GitSource
import Hercules.CNix.Expr (ToRawValue, ViaJSON (ViaJSON))
import Protolude

-- | Documented in @docs/modules/ROOT/pages/evaluation.adoc@.
data HerculesCIMeta = HerculesCIMeta
  { apiBaseUrl :: Text,
    ciSystems :: CISystems,
    pushToBinaryCaches :: BinaryCaches
  }
  deriving (Generic, ToJSON)

-- | Documented in @docs/modules/ROOT/pages/evaluation.adoc@.
data HerculesCIArgs = HerculesCIArgs
  { rev :: Text,
    shortRev :: Text,
    ref :: Text,
    branch :: Maybe Text,
    tag :: Maybe Text,
    primaryRepo :: GitSource,
    herculesCI :: HerculesCIMeta
  }
  deriving (Generic, ToJSON)
  deriving (ToRawValue) via (ViaJSON HerculesCIArgs)

newtype CISystems = CISystems (Maybe (Map Text ()))
  deriving (Generic)
  deriving anyclass (ToJSON)
  deriving (ToRawValue) via (ViaJSON CISystems)

newtype BinaryCaches = BinaryCaches (Maybe (Map Text ()))
  deriving (Generic)
  deriving anyclass (ToJSON)
  deriving (ToRawValue) via (ViaJSON BinaryCaches)

fromGitSource :: GitSource -> HerculesCIMeta -> HerculesCIArgs
fromGitSource primary hci =
  HerculesCIArgs
    { rev = GitSource.rev primary,
      shortRev = GitSource.shortRev primary,
      ref = GitSource.ref primary,
      branch = GitSource.branch primary,
      tag = GitSource.tag primary,
      primaryRepo = primary,
      herculesCI = hci
    }
