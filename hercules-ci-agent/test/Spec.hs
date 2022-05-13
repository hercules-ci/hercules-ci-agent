module Spec
  ( spec,
  )
where

import qualified Hercules.Agent.Nix.RetrieveDerivationInfoSpec
import qualified Hercules.Agent.NixPathSpec
import qualified Hercules.Agent.Worker.ConduitSpec
import qualified Hercules.Agent.Worker.STMSpec
import qualified Hercules.Agent.WorkerProcessSpec
import qualified Hercules.SecretsSpec
import Test.Hspec

spec :: Spec
spec = do
  describe "Hercules.Agent.NixPathSpec" Hercules.Agent.NixPathSpec.spec
  describe "Hercules.Agent.WorkerProcessSpec" Hercules.Agent.WorkerProcessSpec.spec
  describe "Hercules.Agent.Nix.RetrieveDerivationInfo" Hercules.Agent.Nix.RetrieveDerivationInfoSpec.spec
  describe "Hercules.Secret" Hercules.SecretsSpec.spec
  describe "Hercules.Agent.Worker.STMSpec" Hercules.Agent.Worker.STMSpec.spec
  describe "Hercules.Agent.Worker.Conduit" Hercules.Agent.Worker.ConduitSpec.spec
