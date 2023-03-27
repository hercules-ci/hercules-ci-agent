module Spec
  ( spec,
  )
where

import Hercules.Agent.Nix.RetrieveDerivationInfoSpec qualified
import Hercules.Agent.NixPathSpec qualified
import Hercules.Agent.Worker.ConduitSpec qualified
import Hercules.Agent.Worker.STMSpec qualified
import Hercules.Agent.WorkerProcessSpec qualified
import Hercules.SecretsSpec qualified
import Test.Hspec

spec :: Spec
spec = do
  describe "Hercules.Agent.NixPathSpec" Hercules.Agent.NixPathSpec.spec
  describe "Hercules.Agent.WorkerProcessSpec" Hercules.Agent.WorkerProcessSpec.spec
  describe "Hercules.Agent.Nix.RetrieveDerivationInfo" Hercules.Agent.Nix.RetrieveDerivationInfoSpec.spec
  describe "Hercules.Secret" Hercules.SecretsSpec.spec
  describe "Hercules.Agent.Worker.STMSpec" Hercules.Agent.Worker.STMSpec.spec
  describe "Hercules.Agent.Worker.Conduit" Hercules.Agent.Worker.ConduitSpec.spec
