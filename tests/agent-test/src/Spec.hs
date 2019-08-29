module Spec where

import qualified BuildSpec
import qualified EvaluationSpec
import MockTasksApi (ServerHandle)
import Test.Hspec

spec :: SpecWith ServerHandle
spec = do
  EvaluationSpec.spec
  BuildSpec.spec
