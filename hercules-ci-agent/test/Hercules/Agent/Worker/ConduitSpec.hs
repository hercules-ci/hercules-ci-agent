{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Worker.ConduitSpec where

import Data.Conduit
import Data.Conduit.Combinators (sinkList)
import Data.Conduit.List (sourceList)
import Hercules.Agent.Worker.Conduit (tailC, takeCWhileStopEarly, withMessageLimit)
import Protolude hiding (yield)
import Test.Hspec

spec :: Spec
spec = do
  describe "tailC" do
    it "can produce an empty output" do
      l <- runConduit (pass .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` []
    it "can produce a short output (1)" do
      l <- runConduit (sourceList [1] .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` [1]
    it "can produce a short output (2)" do
      l <- runConduit (sourceList [1, 2] .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` [1, 2]
    it "can produce a matching output (3)" do
      l <- runConduit (sourceList [1 .. 3] .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` [1, 2, 3]
    it "can produce a tail output (4)" do
      l <- runConduit (sourceList [1 .. 4] .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` [2, 3, 4]
    it "can produce a tail output (5)" do
      l <- runConduit (sourceList [1 .. 5] .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` [3, 4, 5]
    it "can produce a tail output (100)" do
      l <- runConduit (sourceList [1 .. 100] .| tailC 3 .| sinkList)
      (l :: [Int]) `shouldBe` [98, 99, 100]
  describe "takeCWhileStopEarly" do
    it "works for example" do
      l <- runConduit (sourceList [1 .. 10] .| takeCWhileStopEarly even 2 .| sinkList)
      (l :: [Int]) `shouldBe` [1, 2, 3, 4]
  describe "withMessageLimit" do
    let exampleConduit = do
          withMessageLimit (const True) 20 10 pass (\between -> yield (-1 * between)) yield

    it "works for input 0" do
      r <- runConduit do
        sourceList ([] :: [Int])
          .| exampleConduit
          .| sinkList
      r `shouldBe` []

    it "works for input 1" do
      r <- runConduit do
        sourceList [1 .. 12 :: Int]
          .| exampleConduit
          .| sinkList
      r `shouldBe` [1 .. 12]

    it "works for input 2" do
      r <- runConduit do
        sourceList [1 .. 22 :: Int]
          .| exampleConduit
          .| sinkList
      r `shouldBe` [1 .. 22]

    it "works for input 3" do
      r <- runConduit do
        sourceList [1 .. 30 :: Int]
          .| exampleConduit
          .| sinkList
      r `shouldBe` [1 .. 30]

    it "works for input 4" do
      r <- runConduit do
        sourceList [1 .. 31 :: Int]
          .| exampleConduit
          .| sinkList
      r `shouldBe` [1 .. 20] <> [-1] <> [22 .. 31] <> [1]

    it "works for input 5" do
      r <- runConduit do
        sourceList [1 .. 100 :: Int]
          .| exampleConduit
          .| sinkList
      r `shouldBe` ([1 .. 20] <> [-70] <> [91 .. 100]) <> [70]
