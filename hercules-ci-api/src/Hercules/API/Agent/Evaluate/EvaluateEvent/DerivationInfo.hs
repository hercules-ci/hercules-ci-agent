{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo where

import           Hercules.API.Prelude

type OutputNameText = Text
type DerivationPathText = Text

-- | Derivation fields that are relevant to CI, notably excluding the details
-- that make it buildable and may be sensitive such as the builder script or
-- output hashes.
data DerivationInfo = DerivationInfo
  { derivationPath :: DerivationPathText
  , platform :: Text
  , inputDerivations :: Map DerivationPathText [OutputNameText]
  , inputSources :: [DerivationPathText]
  , outputs :: Map OutputNameText OutputInfo
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data OutputInfo = OutputInfo
  { path :: Text
  , isFixed :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
