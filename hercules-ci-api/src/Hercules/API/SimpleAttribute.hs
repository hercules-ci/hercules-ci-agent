{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.SimpleAttribute
  ( SimpleAttribute (..),
  )
where

import Hercules.API.Prelude
import Prelude ()

-- | NOTE: Generic types must always be wrapped in a newtype, so as to avoid ambiguities in the generated schema.
data SimpleAttribute a = SimpleAttribute
  { path :: [Text],
    value :: a
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, FromJSON, ToJSON)

deriving instance ToSchema a => ToSchema (SimpleAttribute a)

deriving instance Functor SimpleAttribute

deriving instance Foldable SimpleAttribute

deriving instance Traversable SimpleAttribute
