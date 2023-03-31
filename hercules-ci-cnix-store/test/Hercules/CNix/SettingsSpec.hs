{-# LANGUAGE BlockArguments #-}

module Hercules.CNix.SettingsSpec (spec) where

import Hercules.CNix.Settings
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "setUseSQLiteWAL" do
    it "affects getUseSQLiteWAL" do
      withResettingUseSQLiteWAL do
        for_ [True, False] \v -> do
          setUseSQLiteWAL v
          v' <- getUseSQLiteWAL
          v' `shouldBe` v

-- bracket on global state is a leaky abstraction, so we don't add this
-- to the library.
withResettingUseSQLiteWAL :: IO a -> IO a
withResettingUseSQLiteWAL =
  bracket getUseSQLiteWAL setUseSQLiteWAL . const
