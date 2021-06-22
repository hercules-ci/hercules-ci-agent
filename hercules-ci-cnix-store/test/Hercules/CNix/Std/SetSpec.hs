{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.CNix.Std.SetSpec where

import Hercules.CNix.Std.Set as Std.Set
import qualified Language.C.Inline.Cpp as C
import Protolude
import Test.Hspec

C.context stdSetCtx

instanceStdSet "int"
instanceStdSetCopyable "int"

spec :: Spec
spec =
  describe "Std.Set" do
    it "can insert and retrieve copyable values" do
      set <- Std.Set.new
      Std.Set.insert set 1
      Std.Set.insert set 2
      Std.Set.insert set 3
      Std.Set.insert set minBound
      Std.Set.insert set maxBound
      l <- Std.Set.toList set
      sort l `shouldBe` ([minBound, 1, 2, 3, maxBound] :: [C.CInt])
