{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.CurlSpec where

import Data.Text (Text)
import Hercules.CLI.Curl
import Test.Hspec
import Prelude

base :: Text
base = "https://hci.example.com"

spec :: Spec
spec = do
  describe "resolveUrl" do
    it "appends a path to the base URL" \_ -> do
      resolveUrl base "/api/v1/jobs/x" `shouldBe` Right "https://hci.example.com/api/v1/jobs/x"
    it "appends a path to a base URL with a trailing slash" \_ -> do
      resolveUrl "https://hci.example.com/" "/api/v1/jobs/x" `shouldBe` Right "https://hci.example.com/api/v1/jobs/x"
    it "accepts an absolute URL on the base URL's host" \_ -> do
      resolveUrl base "https://hci.example.com/api/v1/jobs/x?y=1" `shouldBe` Right "https://hci.example.com/api/v1/jobs/x?y=1"
    it "compares the host case-insensitively" \_ -> do
      resolveUrl base "https://HCI.example.com/api" `shouldBe` Right "https://HCI.example.com/api"
    it "refuses another host" \_ -> do
      resolveUrl base "https://example.com/api" `shouldBe` Left "URL https://example.com/api is not on https://hci.example.com, so it does not get the token"
    it "refuses a host that only starts with the base URL's host" \_ -> do
      resolveUrl base "https://hci.example.com.evil.example/api" `shouldBe` Left "URL https://hci.example.com.evil.example/api is not on https://hci.example.com, so it does not get the token"
    it "refuses another scheme" \_ -> do
      resolveUrl base "http://hci.example.com/api" `shouldBe` Left "URL http://hci.example.com/api is not on https://hci.example.com, so it does not get the token"
    it "refuses another port" \_ -> do
      resolveUrl base "https://hci.example.com:8443/api" `shouldBe` Left "URL https://hci.example.com:8443/api is not on https://hci.example.com, so it does not get the token"
    it "accepts the scheme's default port written out" \_ -> do
      resolveUrl base "https://hci.example.com:443/api" `shouldBe` Right "https://hci.example.com:443/api"
    it "accepts an omitted port against a base URL with the default port written out" \_ -> do
      resolveUrl "https://hci.example.com:443" "https://hci.example.com/api" `shouldBe` Right "https://hci.example.com/api"
    it "accepts port 80 written out for http" \_ -> do
      resolveUrl "http://hci.example.com" "http://hci.example.com:80/api" `shouldBe` Right "http://hci.example.com:80/api"
    it "refuses port 443 for http" \_ -> do
      resolveUrl "http://hci.example.com" "http://hci.example.com:443/api" `shouldBe` Left "URL http://hci.example.com:443/api is not on http://hci.example.com, so it does not get the token"
    it "refuses user info" \_ -> do
      resolveUrl base "https://u@hci.example.com/api" `shouldBe` Left "URL https://u@hci.example.com/api is not on https://hci.example.com, so it does not get the token"
    it "refuses a host that follows user info looking like the base URL's host" \_ -> do
      resolveUrl base "https://hci.example.com@evil.example/api" `shouldBe` Left "URL https://hci.example.com@evil.example/api is not on https://hci.example.com, so it does not get the token"
    it "keeps the query and fragment of a path" \_ -> do
      resolveUrl base "/api/v1/x/log/lines?logId=1#end" `shouldBe` Right "https://hci.example.com/api/v1/x/log/lines?logId=1#end"
    it "replaces the query of a base URL with the path's" \_ -> do
      resolveUrl "https://hci.example.com/?x=1" "/api?y=2" `shouldBe` Right "https://hci.example.com/api?y=2"
    it "refuses a path that does not parse" \_ -> do
      resolveUrl base "/a b" `shouldBe` Left "URL is not a valid path: /a b"
    it "refuses a reference to another host that starts with //" \_ -> do
      resolveUrl base "//evil.example/api" `shouldBe` Left "URL is not a path: //evil.example/api"
    it "appends a path to a base URL that has a path" \_ -> do
      resolveUrl "https://example.com/hci/" "/api/v1/jobs/x" `shouldBe` Right "https://example.com/hci/api/v1/jobs/x"
    it "refuses a URL that does not parse strictly" \_ -> do
      resolveUrl base "https://hci.example.com\\@evil.example/" `shouldBe` Left "URL is not a valid absolute URL: https://hci.example.com\\@evil.example/"
    it "refuses a relative URL that is not a path" \_ -> do
      resolveUrl base "api/v1" `shouldBe` Left "URL is not a valid absolute URL: api/v1"
  describe "curlArguments" do
    it "passes the header by file descriptor, and the caller's arguments after the URL" \_ -> do
      curlArguments 7 "https://hci.example.com/api" ["-s", "--fail"]
        `shouldBe` ["--globoff", "--header", "@/dev/fd/7", "--url", "https://hci.example.com/api", "-s", "--fail"]
