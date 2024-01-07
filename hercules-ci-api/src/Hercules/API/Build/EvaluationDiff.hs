{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Build.EvaluationDiff
  ( EvaluationDiff (..),
    AttributeDiff (..),
    AttributeValueDiff (..),
    Diff (..),
    IFDDiff (..),
    DerivationOutputNamePair (..),
  )
where

import Data.OpenApi qualified as O3
import Hercules.API.Attribute (Attribute)
import Hercules.API.Derivation (Derivation)
import Hercules.API.Evaluation.AttributeError (AttributeError)
import Hercules.API.Evaluation.Evaluation (Evaluation)
import Hercules.API.Prelude
import Hercules.API.Result (Result)
import Hercules.API.SimpleAttribute (SimpleAttribute)

-- | Generic type for additions, remvals and changes. Addition and removal are
-- represented by nulling the appropriate field.
--
-- This gives the best JSON representation, despite the fact that "Absence" is
-- representable: @{before: null, after: null}@. Most - if not all - endpoints
-- can be expected to not return such a value.
--
-- NOTE: Generic types must always be wrapped in a newtype, so as to avoid
--       ambiguities in the generated schema.
data Diff a = Diff {before :: Maybe a, after :: Maybe a}
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON)

deriving instance (ToSchema a) => ToSchema (Diff a)

newtype AttributeDiff = AttributeDiff (SimpleAttribute AttributeValueDiff)
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

newtype AttributeValueDiff = AttributeValueDiff (Diff (Attribute (Result AttributeError Derivation)))
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

newtype IFDDiff = IFDDiff (Diff DerivationOutputNamePair)
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data DerivationOutputNamePair = DerivationOutputNamePair
  { derivation :: Derivation,
    outputName :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data EvaluationDiff = EvaluationDiff
  { beforeId :: Id Evaluation,
    afterId :: Id Evaluation,
    attributes :: [AttributeDiff],
    ifds :: [IFDDiff]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
