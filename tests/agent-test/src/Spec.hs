module Spec where

import           Test.Hspec
import qualified EvaluationSpec
import           MockTasksApi                   ( ServerHandle )

spec :: SpecWith ServerHandle
spec = EvaluationSpec.spec
