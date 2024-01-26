module Hercules.Formats.MountableSpec (spec) where

import qualified AesonSupport as Aeson
import Data.Aeson (eitherDecode)
import Hercules.Formats.Mountable (Mountable (..))
import Hercules.Formats.Secret (Condition (IsDefaultBranch, Or))
import Hercules.Formats.SecretSpec (genCondition, genText)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary)
import Prelude

spec :: Spec
spec = describe "Mountable" $ do
  Aeson.checkLaws genMountable
  it "Decodes example 1" $
    eitherDecode
      "{ \"source\": \"the source\", \"readOnly\": false, \"condition\": {\"or\": [\"isDefaultBranch\"]} }"
      `shouldBe` Right
        ( Mountable
            { source = "the source",
              readOnly = False,
              condition = Or [IsDefaultBranch]
            }
        )

genMountable :: Gen Mountable
genMountable =
  Mountable <$> genText <*> arbitrary <*> genCondition
