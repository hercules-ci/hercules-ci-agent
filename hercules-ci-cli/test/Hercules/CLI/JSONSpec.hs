{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.CLI.JSONSpec (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Hercules.CLI.JSON
import Protolude
import Test.Hspec

parse :: BL.ByteString -> Either Text Value
parse = f . eitherDecode
  where
    f (Left e) = Left (toS e)
    f (Right r) = Right r

spec :: Spec
spec = do
  describe "mergeObject" do
    it "merges distinct values" \_ -> do
      mergePaths
        [ (["a"], toJSON True),
          (["b"], toJSON [False])
        ]
        `shouldBe` parse "{\"a\": true, \"b\": [false]}"

    it "merges nested values" \_ -> do
      mergePaths
        [ (["x", "a"], toJSON True),
          (["x", "b"], toJSON [False])
        ]
        `shouldBe` parse "{\"x\": {\"a\": true, \"b\": [false]} }"

    it "merges overlapping objects with distinct attrs" \_ -> do
      mergePaths
        [ (["x"], object ["a" .= toJSON True]),
          (["x"], object ["b" .= toJSON [False]])
        ]
        `shouldBe` parse "{\"x\": {\"a\": true, \"b\": [false]} }"

    it "refuses overlapping objects with overlapping attrs" \_ -> do
      mergePaths
        [ (["x"], object ["a" .= toJSON True]),
          (["x"], object ["a" .= toJSON [False]])
        ]
        `shouldBe` Left "Conflicting values for x.a"

    it "merges overlapping objects with distinct attrs" \_ -> do
      mergePaths
        [ (["x"], object ["a" .= toJSON True]),
          ([], object ["x" .= object ["b" .= toJSON [False]]])
        ]
        `shouldBe` parse "{\"x\": {\"a\": true, \"b\": [false]} }"
