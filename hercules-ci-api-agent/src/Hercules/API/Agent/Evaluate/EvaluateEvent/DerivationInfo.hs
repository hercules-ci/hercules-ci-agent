{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo where

import Control.Applicative
import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Hercules.API.Prelude

type OutputNameText = Text

type DerivationPathText = Text

-- | Derivation fields that are relevant to CI, notably excluding the details
-- that make it buildable and may be sensitive such as the builder script or
-- output hashes.
data DerivationInfo
  = DerivationInfo
      { derivationPath :: DerivationPathText,
        platform :: Text,
        requiredSystemFeatures :: [Text],
        inputDerivations :: Map DerivationPathText [OutputNameText],
        inputSources :: [DerivationPathText],
        outputs :: Map OutputNameText OutputInfo
      }
  deriving (Generic, Show, Eq, ToJSON, ToSchema)

instance FromJSON DerivationInfo where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "requiredSystemFeatures" %~ (<|> Just (A.Array mempty))

data OutputInfo
  = OutputInfo
      { path :: Text,
        isFixed :: Bool
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
