{-# LANGUAGE BlockArguments #-}

module Spec where

import qualified Hercules.API.Agent.Effect.EffectTaskSpec
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEventSpec
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec
import qualified Hercules.API.Agent.LifeCycle.AgentInfoSpec
import qualified Hercules.Formats.CachixCacheSpec
import qualified Hercules.Formats.MountableSpec
import qualified Hercules.Formats.SecretSpec
import Test.Hspec

spec :: Spec
spec = describe "hercules-ci-api" do
  describe "Hercules.API.Agent.Effect.EffectTaskSpec" do
    Hercules.API.Agent.Effect.EffectTaskSpec.spec
  describe "Hercules.API.Agent.LifeCycle.AgentInfo" do
    Hercules.API.Agent.LifeCycle.AgentInfoSpec.spec
  describe "Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo" do
    Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfoSpec.spec
  describe "Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent" do
    Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEventSpec.spec
  describe "Hercules.Formats.CachixCache" do
    Hercules.Formats.CachixCacheSpec.spec
  describe "Hercules.Formats.Mountable" do
    Hercules.Formats.MountableSpec.spec
  describe "Hercules.Formats.Secret" do
    Hercules.Formats.SecretSpec.spec
