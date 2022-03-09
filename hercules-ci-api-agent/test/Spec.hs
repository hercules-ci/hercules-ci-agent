{-# LANGUAGE BlockArguments #-}

module Spec where

import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec
import qualified Hercules.API.Agent.LifeCycle.AgentInfoSpec
import qualified Hercules.Formats.CachixCacheSpec
import qualified Hercules.Formats.SecretSpec
import Test.Hspec

spec :: Spec
spec = describe "hercules-ci-api" do
  describe "Hercules.API.Agent.LifeCycle.AgentInfo" do
    Hercules.API.Agent.LifeCycle.AgentInfoSpec.spec
  describe "Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo" do
    Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec.spec
  describe "Hercules.Formats.CachixCache" do
    Hercules.Formats.CachixCacheSpec.spec
  describe "Hercules.Formats.Secret" do
    Hercules.Formats.SecretSpec.spec
