module Spec where

import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec
import qualified Hercules.API.Agent.LifeCycle.AgentInfoSpec
import qualified Hercules.Formats.CachixCacheSpec
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "hercules-ci-api" $ do
  describe "Hercules.API.Agent.LifeCycle.AgentInfo" $
    Hercules.API.Agent.LifeCycle.AgentInfoSpec.spec
  describe "Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo" $
    Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec.spec
  describe "Hercules.Formats.CachixCache" $
    Hercules.Formats.CachixCacheSpec.spec
