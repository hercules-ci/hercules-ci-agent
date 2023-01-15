module Spec where

import BuildSpec qualified
import EvaluationSpec qualified
import MockTasksApi (ServerHandle)
import Test.Hspec

spec :: SpecWith ServerHandle
spec = do
  EvaluationSpec.spec
  BuildSpec.spec
