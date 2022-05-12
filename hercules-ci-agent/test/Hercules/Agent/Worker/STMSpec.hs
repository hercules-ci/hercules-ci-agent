{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Worker.STMSpec (spec) where

import Control.Concurrent.STM (newEmptyTMVarIO, putTMVar, readTMVar)
import Hercules.Agent.Worker.STM
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "asyncIfSTM" do
    it "can create a valid async" do
      fooAsync <-
        asyncIfSTM
          (\_ -> pure (Right "world"))
          \s -> do
            pure ("hi, " <> s)
      hi <- wait fooAsync
      hi `shouldBe` ("hi, world" :: Text)

    it "can reuse an async" do
      withAsync (pure "user") \firstAsync -> do
        fooAsync <-
          asyncIfSTM
            (\_ -> pure (Left firstAsync))
            (panic "do not use")
        hi <- wait fooAsync
        hi `shouldBe` ("user" :: Text)

    it "can write the new async in the STM transaction" do
      asyncHolder <- newEmptyTMVarIO
      _irrelevant <-
        asyncIfSTM
          ( \asy -> do
              putTMVar asyncHolder asy
              pure (Right "user")
          )
          \s -> do
            pure ("hi, " <> s)
      asy <- atomically $ readTMVar asyncHolder
      hi <- wait asy
      hi `shouldBe` ("hi, user" :: Text)
