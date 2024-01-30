module Spec where

import BuildSpec qualified
import EffectSpec qualified
import EvaluationSpec qualified
import MockTasksApi (ServerHandle)
import Test.Hspec

spec :: SpecWith ServerHandle
spec = do
  EffectSpec.spec
  EvaluationSpec.spec
  BuildSpec.spec
