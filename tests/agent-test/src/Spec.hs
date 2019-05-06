module Spec where

import           Test.Hspec
import qualified EvaluationSpec
import qualified BuildSpec
import           MockTasksApi                   ( ServerHandle )

spec :: SpecWith ServerHandle
spec = do
  EvaluationSpec.spec
  BuildSpec.spec
