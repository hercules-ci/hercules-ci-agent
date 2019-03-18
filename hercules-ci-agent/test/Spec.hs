module Spec
    ( spec
    )
where

import           Test.Hspec
import qualified Hercules.Agent.NixPathSpec

spec :: Spec
spec = do
    describe "Hercules.Agent.NixPathSpec" Hercules.Agent.NixPathSpec.spec
