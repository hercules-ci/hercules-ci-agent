module Hercules.API.Agent.LifeCycle.AgentInfoSpec where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Hercules.API.Agent.LifeCycle.AgentInfo
import Test.Hspec
import Prelude

objectV1, objectV2, objectV3 :: AgentInfo
jsonV1, jsonV2, jsonV3 :: BL.ByteString
jsonV1 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"agentVersion\":\"v\",\"cachixPushCaches\":[\"c1\",\"c2\"],\"systemFeatures\":[\"f1\",\"f2\"],\"substituters\":[\"s1\",\"s2\"]}"
objectV1 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = [],
      substituters = ["s1", "s2"],
      concurrentTasks = 2 -- the hardcoded default
    }

jsonV2 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"agentVersion\":\"v\",\"cachixPushCaches\":[\"c1\",\"c2\"],\"systemFeatures\":[\"f1\",\"f2\"],\"substituters\":[\"s1\",\"s2\"], \"concurrentTasks\": 19}"

objectV2 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = [],
      substituters = ["s1", "s2"],
      concurrentTasks = 19 -- something else
    }

jsonV3 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"agentVersion\":\"v\",\"cachixPushCaches\":[\"c1\",\"c2\"],\"pushCaches\":[\"pc1\",\"pc2\"],\"systemFeatures\":[\"f1\",\"f2\"],\"substituters\":[\"s1\",\"s2\"], \"concurrentTasks\": 19}"

objectV3 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = ["pc1", "pc2"],
      substituters = ["s1", "s2"],
      concurrentTasks = 19 -- something else
    }

spec :: Spec
spec = describe "AgentInfo" $ do
  describe "FromJSON" $ do
    it "parses v1 correctly" $ eitherDecode jsonV1 `shouldBe` Right objectV1
    it "parses v2 correctly" $ eitherDecode jsonV2 `shouldBe` Right objectV2
    it "parses v3 correctly" $ eitherDecode jsonV3 `shouldBe` Right objectV3
  describe "ToJSON" $
    it "encodes v2 correctly" $
      eitherDecode (encode objectV2)
        `shouldBe` Right objectV2
  describe "ToJSON" $
    it "encodes v3 correctly" $
      eitherDecode (encode objectV3)
        `shouldBe` Right objectV3
