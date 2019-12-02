{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.Formats.CachixCacheSpec where

import qualified AesonSupport as Aeson
import Data.Aeson (eitherDecode)
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Text (Text)
import Hercules.Formats.CachixCache
import Test.Hspec
import Test.QuickCheck
import Prelude

deriving instance Eq CachixCache

deriving instance Show CachixCache

spec :: Spec
spec = describe "CachixCache" $ do
  Aeson.checkLaws genKeys
  it "Decodes example 'full'" $
    eitherDecode
      "{\"kind\":\"CachixCache\",\"publicKeys\":[\"hello:world\"],\"signingKeys\":[\"DEADBEEF==\"],\"authToken\":\"eyHowdyadoin\"}"
      `shouldBe` Right
        ( CachixCache
            { signingKeys = ["DEADBEEF=="],
              authToken = Just "eyHowdyadoin",
              publicKeys = ["hello:world"]
            }
        )
  it "Decodes example 'no authToken'" $
    eitherDecode
      "{\"kind\":\"CachixCache\",\"publicKeys\":[\"hello:world\"],\"signingKeys\":[\"DEADBEEF==\"]}"
      `shouldBe` Right
        ( CachixCache
            { signingKeys = ["DEADBEEF=="],
              authToken = Nothing,
              publicKeys = ["hello:world"]
            }
        )
  it "Does not decode unknown versions" $
    let r :: Either [Char] CachixCache
        r =
          eitherDecode
            "{\"kind\":\"CachixCache\",\"apiVersion\":\"0.99\",\"publicKeys\":[\"hello:world\"],\"signingKeys\":[\"DEADBEEF==\"]}"
        Left e = r
     in do
          isLeft r `shouldBe` True
          e `shouldContain` "Unexpected apiVersion field."

genKeys :: Gen CachixCache
genKeys =
  resize
    6
    (CachixCache <$> liftArbitrary text <*> liftArbitrary text <*> liftArbitrary text)

text :: Gen Text
text = T.pack <$> resize 4 arbitrary
