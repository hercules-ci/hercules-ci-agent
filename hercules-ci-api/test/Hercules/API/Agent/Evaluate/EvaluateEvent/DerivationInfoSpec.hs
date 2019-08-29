module Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Map (fromList)
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
import Test.Hspec
import Prelude

objectV1, objectV2 :: DerivationInfo

jsonV1, jsonV2 :: BL.ByteString
jsonV1 = "{\"inputDerivations\":{\"/nix/store/1kircip4wskspsqqzxbmh6ss73iqh9ah-bash-4.4-p23.drv\":[\"out\"],\"/nix/store/0si75icim8ajxcsp25d9c52m42kqg1xj-stdenv-linux.drv\":[\"out\"],\"/nix/store/5kpp7mly0qad7l451xhr60k0wbv6vivi-jquery-1.11.3.drv\":[\"out\"],\"/nix/store/kyxayhm3r6gpzbg1d57fwb8m7gkmpwcy-nixos-test-driver-agent-test.drv\":[\"out\"],\"/nix/store/qzvim3ca1s5zxbbvpzzipizfnfpdf5r2-libxslt-1.1.33.drv\":[\"dev\"],\"/nix/store/id49f28qvggmzpmlxp511v6yhc070lkf-jquery-ui-1.11.4.drv\":[\"out\"]},\"platform\":\"x86_64-linux\",\"inputSources\":[\"/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh\",\"/nix/store/fs6a6m9s5n367dslsvsl9lg89h3ns3ya-logfile.css\",\"/nix/store/jjh6h82n5rhw45badlpbj1yv7y6m48h2-log2html.xsl\",\"/nix/store/kdangwwh47ngwxk859ibbvkj4vmm9rzr-treebits.js\"],\"outputs\":{\"out\":{\"path\":\"/nix/store/7vg7ij5933dhg6dh22fvs90bbzmzg8kj-vm-test-run-agent-test\",\"isFixed\":false}},\"derivationPath\":\"testdata/vm-test-run-agent-test.drv\"}"

objectV1 = DerivationInfo {derivationPath = "testdata/vm-test-run-agent-test.drv", platform = "x86_64-linux", requiredSystemFeatures = [], inputDerivations = fromList [("/nix/store/0si75icim8ajxcsp25d9c52m42kqg1xj-stdenv-linux.drv", ["out"]), ("/nix/store/1kircip4wskspsqqzxbmh6ss73iqh9ah-bash-4.4-p23.drv", ["out"]), ("/nix/store/5kpp7mly0qad7l451xhr60k0wbv6vivi-jquery-1.11.3.drv", ["out"]), ("/nix/store/id49f28qvggmzpmlxp511v6yhc070lkf-jquery-ui-1.11.4.drv", ["out"]), ("/nix/store/kyxayhm3r6gpzbg1d57fwb8m7gkmpwcy-nixos-test-driver-agent-test.drv", ["out"]), ("/nix/store/qzvim3ca1s5zxbbvpzzipizfnfpdf5r2-libxslt-1.1.33.drv", ["dev"])], inputSources = ["/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh", "/nix/store/fs6a6m9s5n367dslsvsl9lg89h3ns3ya-logfile.css", "/nix/store/jjh6h82n5rhw45badlpbj1yv7y6m48h2-log2html.xsl", "/nix/store/kdangwwh47ngwxk859ibbvkj4vmm9rzr-treebits.js"], outputs = fromList [("out", OutputInfo {path = "/nix/store/7vg7ij5933dhg6dh22fvs90bbzmzg8kj-vm-test-run-agent-test", isFixed = False})]}

jsonV2 = "{\"inputDerivations\":{\"/nix/store/1kircip4wskspsqqzxbmh6ss73iqh9ah-bash-4.4-p23.drv\":[\"out\"],\"/nix/store/0si75icim8ajxcsp25d9c52m42kqg1xj-stdenv-linux.drv\":[\"out\"],\"/nix/store/5kpp7mly0qad7l451xhr60k0wbv6vivi-jquery-1.11.3.drv\":[\"out\"],\"/nix/store/kyxayhm3r6gpzbg1d57fwb8m7gkmpwcy-nixos-test-driver-agent-test.drv\":[\"out\"],\"/nix/store/qzvim3ca1s5zxbbvpzzipizfnfpdf5r2-libxslt-1.1.33.drv\":[\"dev\"],\"/nix/store/id49f28qvggmzpmlxp511v6yhc070lkf-jquery-ui-1.11.4.drv\":[\"out\"]},\"platform\":\"x86_64-linux\",\"inputSources\":[\"/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh\",\"/nix/store/fs6a6m9s5n367dslsvsl9lg89h3ns3ya-logfile.css\",\"/nix/store/jjh6h82n5rhw45badlpbj1yv7y6m48h2-log2html.xsl\",\"/nix/store/kdangwwh47ngwxk859ibbvkj4vmm9rzr-treebits.js\"],\"outputs\":{\"out\":{\"path\":\"/nix/store/7vg7ij5933dhg6dh22fvs90bbzmzg8kj-vm-test-run-agent-test\",\"isFixed\":false}},\"derivationPath\":\"testdata/vm-test-run-agent-test.drv\",\"requiredSystemFeatures\":[\"kvm\",\"nixos-test\"]}"

objectV2 = DerivationInfo {derivationPath = "testdata/vm-test-run-agent-test.drv", platform = "x86_64-linux", requiredSystemFeatures = ["kvm", "nixos-test"], inputDerivations = fromList [("/nix/store/0si75icim8ajxcsp25d9c52m42kqg1xj-stdenv-linux.drv", ["out"]), ("/nix/store/1kircip4wskspsqqzxbmh6ss73iqh9ah-bash-4.4-p23.drv", ["out"]), ("/nix/store/5kpp7mly0qad7l451xhr60k0wbv6vivi-jquery-1.11.3.drv", ["out"]), ("/nix/store/id49f28qvggmzpmlxp511v6yhc070lkf-jquery-ui-1.11.4.drv", ["out"]), ("/nix/store/kyxayhm3r6gpzbg1d57fwb8m7gkmpwcy-nixos-test-driver-agent-test.drv", ["out"]), ("/nix/store/qzvim3ca1s5zxbbvpzzipizfnfpdf5r2-libxslt-1.1.33.drv", ["dev"])], inputSources = ["/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh", "/nix/store/fs6a6m9s5n367dslsvsl9lg89h3ns3ya-logfile.css", "/nix/store/jjh6h82n5rhw45badlpbj1yv7y6m48h2-log2html.xsl", "/nix/store/kdangwwh47ngwxk859ibbvkj4vmm9rzr-treebits.js"], outputs = fromList [("out", OutputInfo {path = "/nix/store/7vg7ij5933dhg6dh22fvs90bbzmzg8kj-vm-test-run-agent-test", isFixed = False})]}

spec :: Spec
spec = do
  describe "DerivationInfo" $ do
    describe "FromJSON" $ do
      it "parses v1 correctly" $ do
        eitherDecode jsonV1 `shouldBe` Right objectV1
      it "parses v2 correctly" $ do
        eitherDecode jsonV2 `shouldBe` Right objectV2
    describe "ToJSON" $ do
      it "encodes v2 correctly" $ do
        encode objectV2 `shouldBe` jsonV2
