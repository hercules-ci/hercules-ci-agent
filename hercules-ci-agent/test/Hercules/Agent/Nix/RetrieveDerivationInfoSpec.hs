{-# LANGUAGE OverloadedStrings #-}

module Hercules.Agent.Nix.RetrieveDerivationInfoSpec where

import qualified Data.Map as M
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
import Hercules.Agent.Nix.RetrieveDerivationInfo
import Katip (Environment (Environment), initLogEnv, runKatipContextT)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "retrieveDerivationInfo" $ do
    it "parses vm-test-run-agent-test.drv correctly" $ do
      d <- readDrv "testdata/vm-test-run-agent-test.drv"
      derivationPath d `shouldBe` "testdata/vm-test-run-agent-test.drv"
      platform d `shouldBe` "x86_64-linux"
      requiredSystemFeatures d `shouldBe` ["kvm", "nixos-test"]
      inputDerivations d `shouldBe` M.fromList [("/nix/store/0si75icim8ajxcsp25d9c52m42kqg1xj-stdenv-linux.drv", ["out"]), ("/nix/store/1kircip4wskspsqqzxbmh6ss73iqh9ah-bash-4.4-p23.drv", ["out"]), ("/nix/store/5kpp7mly0qad7l451xhr60k0wbv6vivi-jquery-1.11.3.drv", ["out"]), ("/nix/store/id49f28qvggmzpmlxp511v6yhc070lkf-jquery-ui-1.11.4.drv", ["out"]), ("/nix/store/kyxayhm3r6gpzbg1d57fwb8m7gkmpwcy-nixos-test-driver-agent-test.drv", ["out"]), ("/nix/store/qzvim3ca1s5zxbbvpzzipizfnfpdf5r2-libxslt-1.1.33.drv", ["dev"])]
      inputSources d `shouldBe` ["/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh", "/nix/store/fs6a6m9s5n367dslsvsl9lg89h3ns3ya-logfile.css", "/nix/store/jjh6h82n5rhw45badlpbj1yv7y6m48h2-log2html.xsl", "/nix/store/kdangwwh47ngwxk859ibbvkj4vmm9rzr-treebits.js"]
      outputs d `shouldBe` M.fromList [("out", OutputInfo {path = "/nix/store/7vg7ij5933dhg6dh22fvs90bbzmzg8kj-vm-test-run-agent-test", isFixed = False})]

readDrv :: Text -> IO DerivationInfo
readDrv p = do
  le <- initLogEnv mempty (Environment "test")
  runKatipContextT le () mempty $
    retrieveDerivationInfo p
