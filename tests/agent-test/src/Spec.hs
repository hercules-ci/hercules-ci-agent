-- Autogen doesn't work because we have a global test parameter, which
-- is the mock server.
-- {-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}

module Spec where

import           Test.Hspec
import qualified EvaluationSpec
import           MockTasksApi                   ( ServerHandle )

spec :: SpecWith ServerHandle
spec = EvaluationSpec.spec
