{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Build.FailureGraph where

import           Hercules.API.Prelude
import           Hercules.API.Derivation        ( Derivation )

data Graph = Graph
 { nodes :: [Node]
 } deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data Node = Node
 { derivation :: Derivation
 , failedDependencies :: [Text]
 } deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
