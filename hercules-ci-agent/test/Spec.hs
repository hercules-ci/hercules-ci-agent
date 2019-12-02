module Spec
  ( spec,
  )
where

import qualified Hercules.Agent.Nix.RetrieveDerivationInfoSpec
import qualified Hercules.Agent.NixPathSpec
import Test.Hspec

spec :: Spec
spec = do
  describe "Hercules.Agent.NixPathSpec" Hercules.Agent.NixPathSpec.spec
  describe "Hercules.Agent.Nix.RetrieveDerivationInfo" Hercules.Agent.Nix.RetrieveDerivationInfoSpec.spec
