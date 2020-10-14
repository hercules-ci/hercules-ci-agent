{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.CredentialsSpec where

import qualified Data.Map as M
import Hercules.CLI.Credentials
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "urlDomain" do
    it "parses the default" \_ -> do
      urlDomain "https://hercules-ci.com" `shouldBe` Right "hercules-ci.com"
    it "parses away trailing slash" \_ -> do
      urlDomain "https://hercules-ci.com/" `shouldBe` Right "hercules-ci.com"
    it "works with http if you really want" \_ -> do
      urlDomain "http://private-hercules-ci.bizz.biz" `shouldBe` Right "private-hercules-ci.bizz.biz"
    it "reports parse error (1)" \_ -> do
      urlDomain "aburaoernqeg9" `shouldBe` Left "could not parse HERCULES_CI_API_BASE_URL"
    it "reports parse error (2)" \_ -> do
      urlDomain "http:///" `shouldBe` Left "HERCULES_CI_API_BASE_URL domain name must not be empty"
    it "reports parse error (3)" \_ -> do
      urlDomain "http:/" `shouldBe` Left "HERCULES_CI_API_BASE_URL has no domain/authority part"
  describe "parseCredentials" do
    it "parses v0 format (empty)" \_ -> do
      fmap Blind (parseCredentials "path" "{ \"domains\": {} }") `shouldBe` fmap Blind (Right (Credentials mempty))
    it "parses v0 format" \_ -> do
      fmap Blind (parseCredentials "path" "{ \"domains\": { \"hercules-ci.com\": { \"personalToken\": \"tok\" } } }")
        `shouldBe` fmap Blind (Right (Credentials $ M.singleton "hercules-ci.com" (DomainCredentials {personalToken = "tok"})))
