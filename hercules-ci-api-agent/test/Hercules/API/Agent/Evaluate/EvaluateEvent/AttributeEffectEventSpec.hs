module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEventSpec where

import Data.Aeson (Value, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent (AttributeEffectEvent (AttributeEffectEvent), GitToken (MkGitToken), SecretRef (..), SimpleSecret (MkSimpleSecret))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent
import Test.Hspec
import Prelude

objectV1 :: AttributeEffectEvent
objectV1 =
  AttributeEffectEvent
    { expressionPath = ["effects", "doIt"],
      derivationPath = "/nix/store/bar.drv",
      secretsToUse =
        M.fromList
          [ ("foo", (SimpleSecret MkSimpleSecret {name = "default-foo"})),
            ("bar", (GitToken MkGitToken {}))
          ]
    }

jsonV1 :: BL.ByteString
jsonV1 = "{\"derivationPath\":\"/nix/store/bar.drv\",\"expressionPath\":[\"effects\",\"doIt\"],\"secretsToUse\":{\"bar\":{\"contents\":[],\"tag\":\"GitToken\"},\"foo\":{\"contents\":{\"name\":\"default-foo\"},\"tag\":\"SimpleSecret\"}}}"

spec :: Spec
spec = do
  describe "AttributeEventSpec" $ do
    describe "FromJSON" $ do
      it "parses v1 correctly" $ do
        eitherDecode jsonV1 `shouldBe` Right objectV1
    describe "ToJSON" $ do
      it "encodes correctly" $ do
        json (encode objectV1) `shouldBe` json jsonV1

json :: BL.ByteString -> Value
json lbs =
  case eitherDecode lbs of
    Left e -> error e
    Right r -> r
