{-# LANGUAGE BlockArguments #-}

module Hercules.API.AttributeSpec (spec) where

import qualified Data.Text as T
import Hercules.API.Attribute
import Protolude
import Test.Hspec
import Test.QuickCheck

interestingId :: Gen Text
interestingId = T.pack <$> listOf (interestingIdChar)

interestingIdChar :: Gen Char
interestingIdChar =
  frequency
    [ (10, elements ['a' .. 'z']),
      (10, elements ['A' .. 'Z']),
      (10, elements ['0' .. '9']),
      (30, elements "~!@#$%^&*()_+{}|:\"<>?`-=[]\\;',./ "),
      (30, elements "\"\\"),
      (10, arbitrary)
    ]

spec :: Spec
spec = do
  describe "Attribute Path <-> String" do
    it "roundtrips" do
      forAll (listOf (interestingId)) \x ->
        attributePathFromString (attributePathToString x) == x
