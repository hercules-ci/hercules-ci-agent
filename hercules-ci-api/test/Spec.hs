module Spec where

import           Prelude
import           Test.Hspec
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec
import qualified Hercules.Formats.CachixCacheSpec

spec :: Spec
spec = describe "hercules-ci-api" $ do
  describe "Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo" $
     Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec.spec

  describe "Hercules.Formats.CachixCache" $
     Hercules.Formats.CachixCacheSpec.spec
