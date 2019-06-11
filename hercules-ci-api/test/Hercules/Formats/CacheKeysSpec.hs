{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Hercules.Formats.CacheKeysSpec where

import           Prelude

import qualified AesonSupport                  as Aeson
import           Data.Aeson                     ( eitherDecode )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Either                    ( isLeft )
import           Hercules.Formats.CacheKeys
import           Hercules.Formats.CacheKeys.Keys
import           Test.Hspec
import           Test.QuickCheck

deriving instance Eq Keys
deriving instance Eq CacheKeys
deriving instance Show Keys
deriving instance Show CacheKeys

spec :: Spec
spec = describe "CacheKeys" $ do
  Aeson.checkLaws (CacheKeys . M.fromList <$> listOf ((,) <$> text <*> genKeys))
  it "Decodes example 'full'"
    $ eitherDecode
        "{\"caches\":{\"foo\":{\"publicKeys\":[\"hello:world\"],\"signingKeys\":[\"DEADBEEF==\"],\"pullToken\":\"eyHowdyadoin\"}},\"kind\":\"CacheKeys\"}"
    `shouldBe` Right
                 (CacheKeys
                   { caches = M.fromList
                                [ ( "foo"
                                  , Keys { signingKeys = ["DEADBEEF=="]
                                         , pullToken = Just "eyHowdyadoin"
                                         , publicKeys = ["hello:world"]
                                         }
                                  )
                                ]
                   }
                 )
  it "Decodes example 'no pull token'"
    $ eitherDecode
        "{\"caches\":{\"foo\":{\"publicKeys\":[\"hello:world\"],\"signingKeys\":[\"DEADBEEF==\"]}},\"kind\":\"CacheKeys\"}"
    `shouldBe` Right
                 (CacheKeys
                   { caches = M.fromList
                                [ ( "foo"
                                  , Keys { signingKeys = ["DEADBEEF=="]
                                         , pullToken = Nothing
                                         , publicKeys = ["hello:world"]
                                         }
                                  )
                                ]
                   }
                 )
  it "Does not decode unknown versions"
    $ let
        r :: Either [Char] CacheKeys
        r =
          eitherDecode
            "{\"apiVersion\":\"0.99\",\"caches\":{\"foo\":{\"publicKeys\":[\"hello:world\"],\"signingKeys\":[\"DEADBEEF==\"]}},\"kind\":\"CacheKeys\"}"

        Left e = r
      in
        do
          isLeft r `shouldBe` True
          e `shouldContain` "Unexpected apiVersion field."

genKeys :: Gen Keys
genKeys = resize
  6
  (Keys <$> liftArbitrary text <*> liftArbitrary text <*> liftArbitrary text)

text :: Gen Text
text = T.pack <$> resize 4 arbitrary
