{-# LANGUAGE BlockArguments #-}

module Hercules.CNix.ExprSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.CNix.Expr
import qualified Hercules.CNix.Expr.Typed as Typed
import Hercules.CNix.Store.TestUtil (withTempStore)
import Protolude hiding (evalState)
import qualified SingleState
import Test.Hspec

setup :: (Ptr EvalState -> IO a) -> IO a
setup f = f SingleState.evalState

-- withTempStore \store -> withEvalState store f

checkWithNix :: Ptr EvalState -> ByteString -> RawValue -> IO ()
checkWithNix evalState s a = do
  f <- valueFromExpressionString evalState s "/home/someuser/src/dummy-project"
  r <- apply evalState f a
  match evalState r >>= \case
    Right (IsBool b) -> do
      bl <- getBool b
      if bl
        then pass
        else panic $ "Value did not satisfy <<" <> decodeUtf8With lenientDecode s <> ">>"
    Right _ -> panic "wrong type"
    Left e -> throwIO e

spec :: Spec
spec = do
  describe "getLocalFlake" $
    it "gets a trivial flake" $ do
      setup \evalState -> do
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
