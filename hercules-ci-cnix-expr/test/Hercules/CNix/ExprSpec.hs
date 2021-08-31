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

  describe "valueFromExpressionString" do
    it "parses true" do
      setup \evalState -> do
        v <- valueFromExpressionString evalState "true" "/"
        match evalState v >>= \case
          Right (IsBool b) -> do
            r <- getBool b
            r `shouldBe` True
          Right _ -> panic "wrong type"
          Left e -> throwIO e
  describe "toRawValue" do
    it "converts True" $ setup \evalState -> do
      a <- toRawValue evalState True
      a & checkWithNix evalState "a: a == true"
    it "converts False" $ setup \evalState -> do
      a <- toRawValue evalState False
      a & checkWithNix evalState "a: a == false"
    it "converts 0" $ setup \evalState -> do
      a <- toRawValue evalState (0 :: Int64)
      a & checkWithNix evalState "a: a == 0"
    it "converts 1" $ setup \evalState -> do
      a <- toRawValue evalState (1 :: Int64)
      a & checkWithNix evalState "a: a == 1"
    it "converts 42" $ setup \evalState -> do
      a <- toRawValue evalState (42 :: Int64)
      a & checkWithNix evalState "a: a == 42"
    -- Oddly, Nix does not support -9223372036854775808, which would be the
    -- most negative int64
    it "converts -9223372036854775807" $ setup \evalState -> do
      a <- toRawValue evalState (-9223372036854775807 :: Int64)
      a & checkWithNix evalState "a: a == (-9223372036854775807)"
    it "converts 9223372036854775807" $ setup \evalState -> do
      a <- toRawValue evalState (9223372036854775807 :: Int64)
      a & checkWithNix evalState "a: a == 9223372036854775807"
    it "converts Map.empty" $ setup \evalState -> do
      a <- toRawValue evalState (mempty :: Map ByteString Int64)
      a & checkWithNix evalState "a: a == {}"
    it "converts Map.singleton" $ setup \evalState -> do
      a <- toRawValue evalState (M.singleton "foo" 1 :: Map ByteString Int64)
      a & checkWithNix evalState "a: a == { foo = 1; }"
    it "converts empty bytes" $ setup \evalState -> do
      a <- toRawValue evalState ("" :: ByteString)
      a & checkWithNix evalState "a: a == ''''"
    it "converts empty text" $ setup \evalState -> do
      a <- toRawValue evalState ("" :: Text)
      a & checkWithNix evalState "a: a == ''''"
    it "converts bytes" $ setup \evalState -> do
      a <- toRawValue evalState ("hi" :: ByteString)
      a & checkWithNix evalState "a: a == ''hi''"
    it "converts text" $ setup \evalState -> do
      a <- toRawValue evalState ("hi" :: Text)
      a & checkWithNix evalState "a: a == ''hi''"
    it "converts empty list" $ setup \evalState -> do
      a <- toRawValue evalState ([] :: [Int])
      a & checkWithNix evalState "a: a == []"
    it "converts singleton list" $ setup \evalState -> do
      a <- toRawValue evalState ["hi" :: Text]
      a & checkWithNix evalState "a: a == [''hi'']"
    it "converts list" $ setup \evalState -> do
      a <- toRawValue evalState =<< sequenceA [toRawValue evalState ("hi" :: Text), toRawValue evalState True, toRawValue evalState (1 :: Int), toRawValue evalState (mempty :: Map ByteString Int)]
      a & checkWithNix evalState "a: a == [''hi'' true 1 {}]"
    it "converts json" $ setup \evalState -> do
      jsonExample <- BS.readFile "test/data/sample.json"
      jsonValue <- case A.eitherDecode (BL.fromStrict jsonExample) of
        Left e -> panic (toS e)
        Right r -> pure (r :: A.Value)
      a <- toRawValue evalState jsonValue
      a & checkWithNix evalState ("a: a == builtins.fromJSON ''" <> jsonExample <> "''")
