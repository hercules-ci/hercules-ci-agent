module Spec where

import           Prelude
import           Test.Hspec
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec

spec :: Spec
spec = describe "hercules-ci-api" $
  describe "Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec" $
     Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec.spec
