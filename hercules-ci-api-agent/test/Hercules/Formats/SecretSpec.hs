{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}

module Hercules.Formats.SecretSpec where

import qualified AesonSupport as Aeson
import Control.Applicative
import Data.Aeson (eitherDecode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import Data.Char
import Data.Either (isLeft)
import Data.Int
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Hercules.Formats.Secret
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes ()
import Prelude

deriving instance Eq Secret

deriving instance Show Secret

spec :: Spec
spec = describe "Secret" $ do
  Aeson.checkLaws genSecret
  it "Decodes example 1" $
    eitherDecode
      "{\"kind\":\"Secret\",\"data\":{\"key\": \"s3cr3t\"}}"
      `shouldBe` Right
        ( Secret
            { data_ = M.singleton "key" "s3cr3t",
              condition = Nothing
            }
        )
  it "Decodes example 2" $ do
    eitherDecode
      "{\"kind\":\"Secret\",\"data\":{\"key\": \"s3cr3t\"},\"condition\":{\"or\": [\"isDefaultBranch\", {\"isBranch\": \"master\"}, {\"isBranch\": \"production\"}]}}"
      `shouldBe` Right
        ( Secret
            { data_ = M.singleton "key" "s3cr3t",
              condition = Just $ Or [IsDefaultBranch, IsBranch "master", IsBranch "production"]
            }
        )
  it "Does not decode unknown versions" $
    let r :: Either [Char] Secret
        r =
          eitherDecode
            "{\"kind\":\"Secret\",\"apiVersion\":\"0.99\",\"data\":{}}"
        Left e = r
     in do
          isLeft r `shouldBe` True
          e `shouldContain` "Unexpected apiVersion field."

genSecret :: Gen Secret
genSecret =
  resize
    6
    ( Secret
        <$> liftArbitraryMap text (resize 100 genValue)
        <*> frequency [(1, pure Nothing), (19, Just <$> genCondition)]
    )

genCondition :: Gen Condition
genCondition =
  frequency
    [ (1, Or <$> resize 3 (listOf genCondition)),
      (1, And <$> resize 3 (listOf genCondition)),
      (1, pure IsDefaultBranch),
      (1, pure IsTag),
      (1, IsBranch <$> text),
      (1, IsRepo <$> text),
      (1, IsOwner <$> text)
    ]

text :: Gen Text
text = TE.decodeUtf8 . TE.encodeUtf8 . T.pack <$> resize 4 arbitrary

liftArbitraryMap :: (Ord k) => Gen k -> Gen a -> Gen (M.Map k a)
liftArbitraryMap k v = M.fromList <$> liftArbitrary ((,) <$> k <*> v)

genValue :: Gen A.Value
genValue = sized \m -> do
  n <- choose (0, m `div` 2)
  let n' = m `div` 4
  frequency
    [ (2, A.String <$> text),
      (2, A.Array . V.fromList <$> vectorOf n (resize n' genValue)),
      (5, A.object <$> vectorOf n ((\s k -> AK.fromText s A..= k) <$> text <*> resize n' genValue)),
      (1, A.Number . fromIntegral <$> (arbitrary :: Gen Int64)), -- no fractions :( but doesn't matter here
      (1, A.Bool <$> arbitrary)
    ]
