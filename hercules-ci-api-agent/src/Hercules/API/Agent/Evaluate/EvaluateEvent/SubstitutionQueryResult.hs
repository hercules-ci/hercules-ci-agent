{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.SubstitutionQueryResult
  ( SubstitutionQueryResult (..),
  )
where

import Hercules.API.Agent.OutputInfo (OutputInfo)
import Hercules.API.Prelude

data SubstitutionQueryResult = SubstitutionQueryResult
  { storeURI :: !Text,
    -- | NB: does not necessarily match outputInfo.derivation
    derivation :: !Text,
    -- | NB: does not necessarily match outputInfo.name
    outputName :: !Text,
    -- | NB: does not necessarily match derivation and outputName
    --
    -- 'Nothing': not substitutable
    -- 'Just': substitutable, and expect the following metadata
    outputInfo :: !(Maybe OutputInfo)
  }
  deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON)
