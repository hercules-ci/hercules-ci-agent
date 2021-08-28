{-# LANGUAGE BlockArguments #-}

module Hercules.CNix.ExprSpec (spec) where

import Hercules.CNix.Expr
import qualified Hercules.CNix.Expr.Typed as Typed
import Hercules.CNix.Store.TestUtil (withTempStore)
import Protolude hiding (evalState)
import Test.Hspec

spec :: Spec
spec = do
  describe "getLocalFlake" $
    it "gets a trivial flake" $ withTempStore \store -> do
      withEvalState store \evalState -> do
        v <- getLocalFlake evalState "test/data/simple-flake"
        lib <- getAttr evalState v "lib"
        libAttrs <-
          for lib (Typed.match evalState) >>= \case
            Just (Right (Typed.IsAttrs as)) -> pure as
            Just (Left e) -> panic $ "lib error " <> show e
            Just _ -> panic "lib must be attrs"
            Nothing -> panic "no lib???"
        greeting <- getAttr evalState libAttrs "greeting"
        greetingBytes <-
          for greeting (Typed.match evalState) >>= \case
            Just (Right (Typed.IsString s)) -> getStringIgnoreContext s
            Just (Left e) -> panic $ "lib.greeting error " <> show e
            Just _ -> panic "lib.greeting must be string"
            Nothing -> panic "no lib.greeting???"
        greetingBytes `shouldBe` "hello flake"
