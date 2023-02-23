{-# LANGUAGE BlockArguments #-}

module Spec where

import qualified Hercules.API.AttributeSpec
import Test.Hspec

spec :: Spec
spec = describe "hercules-ci-api" do
  describe "Hercules.API.Attribute" do
    Hercules.API.AttributeSpec.spec
