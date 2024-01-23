{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.FailureGraph where

import Data.OpenApi qualified as O3
import Hercules.API.Derivation (Derivation)
import Hercules.API.Prelude

-- | A graph where each node is a failed (directly/indirectly) derivation
--   and edges represent a dependencies on other directly or indirectly failed
--   derivations.
data Graph = Graph
  { nodes :: [Node]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

-- | A derivation and any dependencies that caused it to fail, if applicable.
data Node = Node
  { derivation :: Derivation,
    -- | Dependency paths to failed dependencies that, if present, have caused a
    --   DependencyFailure for this derivation.
    failedDependencies :: [Text]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
