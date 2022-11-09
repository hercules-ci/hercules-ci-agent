{-# LANGUAGE BlockArguments #-}

module Hercules.API.Agent.Effect.EffectTaskSpec where

import Data.Aeson (Value, eitherDecode, encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.API.Agent.Effect.EffectTask (EffectTask (..))
import Hercules.API.Id (Id (Id))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

objectV1 :: EffectTask
objectV1 =
  EffectTask
    { id = Id (read "ba2cb3c7-a234-4688-bd25-981a0ccc1497"),
      derivationPath = "/nix/store/foo.drv",
      logToken = "it me logToken",
      inputDerivationOutputPaths = ["/nix/store/bar-bin", "/nix/store/qux-dev"],
      token = "the task token",
      serverSecrets = mempty,
      projectId = Id (read "b85bfc97-86e2-48fc-8153-72d05687eed2"),
      projectPath = "github/the-org/the-repo",
      siteName = "github",
      ownerName = "the-org",
      repoName = "the-repo",
      ref = "refs/heads/the-feature",
      isDefaultBranch = True
    }

objectV2 :: EffectTask
objectV2 = objectV1 {serverSecrets = M.singleton "hi" (M.singleton "token" (A.String "hi token"))}

jsonV1 :: BL.ByteString
jsonV1 = "{\"derivationPath\":\"/nix/store/foo.drv\",\"id\":\"ba2cb3c7-a234-4688-bd25-981a0ccc1497\",\"inputDerivationOutputPaths\":[\"/nix/store/bar-bin\",\"/nix/store/qux-dev\"],\"isDefaultBranch\":true,\"logToken\":\"it me logToken\",\"ownerName\":\"the-org\",\"projectId\":\"b85bfc97-86e2-48fc-8153-72d05687eed2\",\"projectPath\":\"github/the-org/the-repo\",\"ref\":\"refs/heads/the-feature\",\"repoName\":\"the-repo\",\"siteName\":\"github\",\"token\":\"the task token\"}"

jsonV2 :: BL.ByteString
jsonV2 = "{\"derivationPath\":\"/nix/store/foo.drv\",\"id\":\"ba2cb3c7-a234-4688-bd25-981a0ccc1497\",\"inputDerivationOutputPaths\":[\"/nix/store/bar-bin\",\"/nix/store/qux-dev\"],\"isDefaultBranch\":true,\"logToken\":\"it me logToken\",\"ownerName\":\"the-org\",\"projectId\":\"b85bfc97-86e2-48fc-8153-72d05687eed2\",\"projectPath\":\"github/the-org/the-repo\",\"ref\":\"refs/heads/the-feature\",\"repoName\":\"the-repo\",\"siteName\":\"github\",\"token\":\"the task token\",\"serverSecrets\":{\"hi\":{\"token\":\"hi token\"}}}"

spec :: Spec
spec = do
  describe "AttributeEventSpec" do
    describe "FromJSON" do
      it "parses v1 correctly" do
        eitherDecode jsonV1 `shouldBe` Right objectV1
      it "parses v2 correctly" do
        eitherDecode jsonV2 `shouldBe` Right objectV2
    describe "ToJSON" do
      it "encodes correctly" do
        json (encode objectV2) `shouldBe` json jsonV2

json :: BL.ByteString -> Value
json lbs =
  case eitherDecode lbs of
    Left e -> Prelude.error e
    Right r -> r
