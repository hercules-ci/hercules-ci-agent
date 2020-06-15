module Hercules.Agent.WorkerProcessSpec (spec) where

import qualified Data.Map as M
import Hercules.API.Agent.Evaluate.EvaluateTask
import Hercules.Agent.WorkerProcess
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "modifyEnv" $ do
    let baseEnvSettings = WorkerEnvSettings []
        baseEnv = M.fromList [("NIX_PATH", "")]
    it "sets NIX_PATH" $ \() -> do
      modifyEnv (WorkerEnvSettings [NixPathElement (Just "a") $ SubPathOf "/b" Nothing]) mempty
        `shouldBe` M.fromList [("NIX_PATH", "a=/b")]
    it "filters out NIXPKGS_CONFIG" $ do
      modifyEnv baseEnvSettings ("NIXPKGS_CONFIG" =: "abpuhasur")
        `shouldBe` baseEnv
    it "filters out NIXOS_EXTRA_MODULES" $ do
      modifyEnv baseEnvSettings ("NIXOS_EXTRA_MODULES" =: "a9epr8gabru")
        `shouldBe` baseEnv
    it "filters out IN_NIX_SHELL" $ do
      modifyEnv baseEnvSettings ("IN_NIX_SHELL" =: "sa9e5h8")
        `shouldBe` baseEnv
    it "preserves other vars" $ do
      modifyEnv baseEnvSettings ("IN_NIX_SHELL" =: "sa9e5h8" <> "LD_LIBRARY_PATH" =: "bad idea but works")
        `shouldBe` baseEnv <> "LD_LIBRARY_PATH" =: "bad idea but works"

(=:) :: k -> a -> Map k a
(=:) = M.singleton
