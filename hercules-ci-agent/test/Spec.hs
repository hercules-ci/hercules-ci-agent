module Spec
  ( spec
  )
where

import           Test.Hspec
import qualified Hercules.Agent.NixPathSpec
import qualified Hercules.Agent.Nix.RetrieveDerivationInfoSpec

spec :: Spec
spec = do
  describe "Hercules.Agent.NixPathSpec" Hercules.Agent.NixPathSpec.spec
  describe "Hercules.Agent.Nix.RetrieveDerivationInfo" Hercules.Agent.Nix.RetrieveDerivationInfoSpec.spec