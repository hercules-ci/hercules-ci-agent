{-# LANGUAGE BlockArguments #-}

module Hercules.CNix.VerbositySpec (spec) where

import Hercules.CNix.Verbosity
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "setVerbosity" do
    it "affects getVerbosity" do
      withResettingVerbosity do
        for_ [minBound .. maxBound] \v -> do
          setVerbosity v
          v' <- getVerbosity
          v' `shouldBe` v
  describe "setShowTrace" do
    it "affects getShowTrace" do
      withResettingShowTrace do
        for_ [True, False] \v -> do
          setShowTrace v
          v' <- getShowTrace
          v' `shouldBe` v

-- bracket on global state is still a leaky abstraction, so we don't add this
-- to the library.
withResettingVerbosity :: IO a -> IO a
withResettingVerbosity =
  bracket getVerbosity setVerbosity . const

withResettingShowTrace :: IO a -> IO a
withResettingShowTrace =
  bracket getShowTrace setShowTrace . const
