{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Build.FailureGraph where

import           Hercules.API.Prelude
import           Hercules.API.Derivation        ( Derivation )

-- | A graph where each node is a failed (directly/indirectly) derivation
--   and edges represent a dependencies on other directly or indirectly failed
--   derivations.
data Graph = Graph
 { nodes :: [Node]
 } deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | A derivation and any dependencies that caused it to fail, if applicable.
data Node = Node
 { derivation :: Derivation
 , failedDependencies :: [Text]
 -- ^ Dependency paths to failed dependencies that, if present, have caused a
 --   DependencyFailure for this derivation.
 } deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
