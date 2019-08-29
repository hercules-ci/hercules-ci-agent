{-# LANGUAGE OverloadedStrings #-}

module Hercules.Agent.NixPathSpec where

import Hercules.API.Agent.Evaluate.EvaluateTask
import Hercules.Agent.NixPath
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "renderNixPath" $ do
    it "renders a=/b" $ \() -> do
      renderNixPath [NixPathElement (Just "a") $ SubPathOf "/b" Nothing]
        `shouldBe` "a=/b"
    it "renders a=/b/c (subpath)" $ \() -> do
      renderNixPath [NixPathElement (Just "a") $ SubPathOf "/b" $ Just "c"]
        `shouldBe` "a=/b/c"
    it "renders /b" $ \() -> do
      renderNixPath [NixPathElement Nothing $ SubPathOf "/b" Nothing]
        `shouldBe` "/b"
    it "renders /b/c" $ \() -> do
      renderNixPath [NixPathElement Nothing $ SubPathOf "/b" $ Just "c"]
        `shouldBe` "/b/c"
    it "renders a=/b:d=/e" $ \() -> do
      renderNixPath
        [ NixPathElement (Just "a") $ SubPathOf "/b" Nothing,
          NixPathElement (Just "d") $ SubPathOf "/e" Nothing
          ]
        `shouldBe` "a=/b:d=/e"
