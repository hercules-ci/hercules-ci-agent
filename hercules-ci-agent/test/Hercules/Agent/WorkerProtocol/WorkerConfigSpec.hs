{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.WorkerProtocol.WorkerConfigSpec where

import Data.Aeson qualified as A
import Hercules.Agent.WorkerProtocol.WorkerConfig
import Protolude
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "WorkerConfig" do
    it "can be serialized and deserialized" do
      pending
  describe "ViaShowRead" do
    it "roundtrips aeson for Int" do
      property \i -> do
        let v = ViaShowRead (i :: Int)
        Just v === (A.decode . A.encode) v
    it "fails for invalid input" do
      e <- shouldBeLeft $ A.eitherDecode @(ViaShowRead Int) "\"invalid\""
      e `shouldContain` "Could not parse \"invalid\""

shouldBeLeft :: forall b a. Either a b -> IO a
shouldBeLeft (Left a) = pure a
shouldBeLeft (Right _) = assertFailure "Expected Left, got Right"
