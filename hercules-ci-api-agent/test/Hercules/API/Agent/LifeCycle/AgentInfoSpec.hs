module Hercules.API.Agent.LifeCycle.AgentInfoSpec where

import Data.Aeson (eitherDecode, encode, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Text (Text)
import Hercules.API.Agent.LifeCycle.AgentInfo
import Test.Hspec
import Prelude

objectV1, objectV2, objectV3, objectV4 :: AgentInfo
jsonV1, jsonV2, jsonV3, jsonV4 :: BL.ByteString
jsonV1 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"agentVersion\":\"v\",\"cachixPushCaches\":[\"c1\",\"c2\"],\"systemFeatures\":[\"f1\",\"f2\"],\"substituters\":[\"s1\",\"s2\"]}"
objectV1 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      nixClientProtocolVersion = 0,
      nixDaemonProtocolVersion = 0,
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = [],
      substituters = ["s1", "s2"],
      concurrentTasks = 2, -- the hardcoded default
      labels = mempty
    }

jsonV2 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"agentVersion\":\"v\",\"cachixPushCaches\":[\"c1\",\"c2\"],\"systemFeatures\":[\"f1\",\"f2\"],\"substituters\":[\"s1\",\"s2\"], \"concurrentTasks\": 19}"

objectV2 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      nixClientProtocolVersion = 0,
      nixDaemonProtocolVersion = 0,
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = [],
      substituters = ["s1", "s2"],
      concurrentTasks = 19,
      labels = mempty
    }

jsonV3 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"agentVersion\":\"v\",\"cachixPushCaches\":[\"c1\",\"c2\"],\"pushCaches\":[\"pc1\",\"pc2\"],\"systemFeatures\":[\"f1\",\"f2\"],\"substituters\":[\"s1\",\"s2\"], \"concurrentTasks\": 19}"

objectV3 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      nixClientProtocolVersion = 0,
      nixDaemonProtocolVersion = 0,
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = ["pc1", "pc2"],
      substituters = ["s1", "s2"],
      concurrentTasks = 19,
      labels = mempty
    }

jsonV4 = "{\"platforms\":[\"riscv-sel4\"],\"hostname\":\"a\",\"nixVersion\":\"nv\",\"concurrentTasks\":19,\"agentVersion\":\"v\",\"labels\":{\"an object\":{\"hi\":42},\"drinks menu\":[\"goat milk\",\"mate\",\"matcha\"],\"nix is cool\":true,\"revision\":\"deadbeef\"},\"cachixPushCaches\":[\"c1\",\"c2\"],\"pushCaches\":[\"pc1\",\"pc2\"],\"nixDaemonProtocolVersion\":274,\"systemFeatures\":[\"f1\",\"f2\"],\"nixClientProtocolVersion\":270,\"substituters\":[\"s1\",\"s2\"]}"

objectV4 =
  AgentInfo
    { hostname = "a",
      agentVersion = "v",
      nixVersion = "nv",
      nixClientProtocolVersion = 270,
      nixDaemonProtocolVersion = 274,
      platforms = ["riscv-sel4"],
      systemFeatures = ["f1", "f2"],
      cachixPushCaches = ["c1", "c2"],
      pushCaches = ["pc1", "pc2"],
      substituters = ["s1", "s2"],
      concurrentTasks = 19,
      labels =
        M.fromList
          [ "nix is cool" .= True,
            "revision" .= ("deadbeef" :: Text),
            "an object"
              .= A.object
                [ "hi" .= (42 :: Int)
                ],
            "drinks menu" .= ["goat milk", "mate", "matcha" :: Text]
          ]
    }

spec :: Spec
spec = describe "AgentInfo" $ do
  describe "FromJSON" $ do
    it "parses v1 correctly" $ eitherDecode jsonV1 `shouldBe` Right objectV1
    it "parses v2 correctly" $ eitherDecode jsonV2 `shouldBe` Right objectV2
    it "parses v3 correctly" $ eitherDecode jsonV3 `shouldBe` Right objectV3
    it "parses v4 correctly" $ eitherDecode jsonV4 `shouldBe` Right objectV4
  describe "ToJSON" $ do
    it "encodes v2 correctly" $
      eitherDecode (encode objectV2)
        `shouldBe` Right objectV2
    it "encodes v3 correctly" $
      eitherDecode (encode objectV3)
        `shouldBe` Right objectV3
    it "encodes v4 correctly" $
      eitherDecode (encode objectV3)
        `shouldBe` Right objectV3
