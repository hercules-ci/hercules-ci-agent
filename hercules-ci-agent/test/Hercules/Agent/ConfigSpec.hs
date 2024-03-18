{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.ConfigSpec (spec) where

import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.Text qualified as T
import Hercules.Agent.Config (Config (..), Purpose (Input), combiCodec)
import Hercules.Agent.Config.Combined (forJson, forToml)
import Hercules.Agent.Config.Json qualified as Json
import Hercules.CNix.Verbosity (Verbosity (Vomit))
import Hercules.Formats.Mountable (Mountable (Mountable))
import Hercules.Formats.Mountable qualified
import Hercules.Formats.Secret qualified
import Katip (Severity (DebugS))
import Protolude
import Test.Hspec
import Toml.Codec.Code

deriving instance Eq (Config 'Input)

deriving instance Show (Config 'Input)

spec :: Spec
spec =
  describe "Config" $ do
    describe "toml codec" $ do
      -- note that this hooks into a more strict version of the parser
      it "parsers example 1 strictly" $ do
        let input =
              T.unlines
                [ "allowInsecureBuiltinFetchers = true",
                  "apiBaseUrl = \"http://api\"",
                  "baseDirectory = \"/var/lib/hercules-ci-agent\"",
                  "binaryCachesPath = \"/nix/store/9cbyq6f2ajxm64rvffhy8ndi73mbg5zd-binary-caches.json\"",
                  "clusterJoinTokenPath = \"/nix/store/y0200pr82sczzla5jd4pzkb8idnqhxkj-pretend-agent-token\"",
                  "concurrentTasks = 4",
                  "logLevel = \"DebugS\"",
                  "nixUserIsTrusted = true",
                  "secretsJsonPath = \"/var/lib/hercules-ci-agent/secrets/secrets.json\"",
                  "staticSecretsDirectory = \"/var/lib/hercules-ci-agent/secrets\"",
                  "workDirectory = \"/var/lib/hercules-ci-agent/work\"",
                  "remotePlatformsWithSameFeatures = [\"aarch64-darwin\"]",
                  "nixVerbosity = \"vomit\"",
                  "[nixSettings]",
                  "  substituters = \"https://example.com\"",
                  -- This line should not be needed!
                  "[effectMountables]",
                  "[effectMountables.hosts]",
                  "  condition = true",
                  "  readOnly = true",
                  "  source = \"/etc/hosts\"",
                  "[labels]",
                  "module = \"nixos-profile\"",
                  "[labels.agent]",
                  "source = \"flake\"",
                  "[labels.lib]",
                  "version = \"24.05pre-git\""
                ]
        decode (forToml combiCodec) input
          `shouldBe` Right
            ( Config
                { herculesApiBaseURL = Just "http://api",
                  nixUserIsTrusted = Just True,
                  concurrentTasks = Just (Right 4),
                  baseDirectory = Just "/var/lib/hercules-ci-agent",
                  staticSecretsDirectory = Just "/var/lib/hercules-ci-agent/secrets",
                  workDirectory = Just "/var/lib/hercules-ci-agent/work",
                  clusterJoinTokenPath = Just "/nix/store/y0200pr82sczzla5jd4pzkb8idnqhxkj-pretend-agent-token",
                  binaryCachesPath = Just "/nix/store/9cbyq6f2ajxm64rvffhy8ndi73mbg5zd-binary-caches.json",
                  secretsJsonPath = Just "/var/lib/hercules-ci-agent/secrets/secrets.json",
                  logLevel = Just DebugS,
                  nixVerbosity = Nothing, -- FIXME: should be Just "vomit"
                  labels =
                    Just
                      ( M.fromList
                          [ ("agent", (A.object [("source", A.String "flake")])),
                            ("lib", A.object ([("version", A.String "24.05pre-git")])),
                            ("module", A.String "nixos-profile")
                          ]
                      ),
                  allowInsecureBuiltinFetchers = Just True,
                  remotePlatformsWithSameFeatures = Just ["aarch64-darwin"],
                  effectMountables =
                    M.fromList
                      [ ( "hosts",
                          Mountable
                            { source = "/etc/hosts",
                              readOnly = True,
                              condition = Hercules.Formats.Secret.Const True
                            }
                        )
                      ],
                  nixSettings = M.singleton "substituters" "https://example.com"
                }
            )
      it "parses empty config" $ do
        let input = T.unlines []
        decode (forToml combiCodec) input
          `shouldBe` Right
            emptyConfig
              { -- sure, why not
                labels = Just mempty
              }

    describe "json codec" $ do
      it "parses example 1" $ do
        let input =
              T.unlines
                [ "{",
                  "\"apiBaseUrl\": \"http://api\",",
                  "\"nixUserIsTrusted\": true,",
                  "\"concurrentTasks\": 4,",
                  "\"baseDirectory\": \"/var/lib/hercules-ci-agent\",",
                  "\"staticSecretsDirectory\": \"/var/lib/hercules-ci-agent/secrets\",",
                  "\"workDirectory\": \"/var/lib/hercules-ci-agent/work\",",
                  "\"clusterJoinTokenPath\": \"/nix/store/y0200pr82sczzla5jd4pzkb8idnqhxkj-pretend-agent-token\",",
                  "\"binaryCachesPath\": \"/nix/store/9cbyq6f2ajxm64rvffhy8ndi73mbg5zd-binary-caches.json\",",
                  "\"secretsJsonPath\": \"/var/lib/hercules-ci-agent/secrets/secrets.json\",",
                  "\"logLevel\": \"DebugS\",",
                  "\"nixVerbosity\": \"Vomit\",",
                  "\"nixSettings\": { \"substituters\": \"https://example.com\" },",
                  "\"remotePlatformsWithSameFeatures\": [\"aarch64-darwin\"],",
                  "\"labels\": {",
                  "  \"agent\": {\"source\": \"flake\"},",
                  "  \"lib\": {\"version\": \"24.05pre-git\"},",
                  "  \"module\": \"nixos-profile\"",
                  "},",
                  "\"effectMountables\": {",
                  "  \"hosts\": {",
                  "    \"source\": \"/etc/hosts\",",
                  "    \"readOnly\": true,",
                  "    \"condition\": true,",
                  "    \"and\":[]",
                  "  }",
                  "},",
                  "\"allowInsecureBuiltinFetchers\": true",
                  "}"
                ]
        Json.decode (forJson combiCodec) (toS $ encodeUtf8 input)
          `shouldBe` Right
            Config
              { herculesApiBaseURL = Just "http://api",
                nixUserIsTrusted = Just True,
                concurrentTasks = Just (Right 4),
                baseDirectory = Just "/var/lib/hercules-ci-agent",
                staticSecretsDirectory = Just "/var/lib/hercules-ci-agent/secrets",
                workDirectory = Just "/var/lib/hercules-ci-agent/work",
                clusterJoinTokenPath = Just "/nix/store/y0200pr82sczzla5jd4pzkb8idnqhxkj-pretend-agent-token",
                binaryCachesPath = Just "/nix/store/9cbyq6f2ajxm64rvffhy8ndi73mbg5zd-binary-caches.json",
                secretsJsonPath = Just "/var/lib/hercules-ci-agent/secrets/secrets.json",
                logLevel = Just DebugS,
                nixVerbosity = Just Vomit,
                labels =
                  Just
                    ( M.fromList
                        [ ("agent", (A.object [("source", A.String "flake")])),
                          ("lib", A.object ([("version", A.String "24.05pre-git")])),
                          ("module", A.String "nixos-profile")
                        ]
                    ),
                allowInsecureBuiltinFetchers = Just True,
                remotePlatformsWithSameFeatures = Just ["aarch64-darwin"],
                effectMountables =
                  M.fromList
                    [ ( "hosts",
                        Mountable
                          { source = "/etc/hosts",
                            readOnly = True,
                            condition = Hercules.Formats.Secret.Const True
                          }
                      )
                    ],
                nixSettings = M.singleton "substituters" "https://example.com"
              }

      it "handles empty config" $ do
        let inputs =
              T.unlines
                [ "{",
                  "}"
                ]
        Json.decode (forJson combiCodec) (toS $ encodeUtf8 inputs) `shouldBe` Right emptyConfig

      it "handles nulls" $ do
        let inputs =
              T.unlines
                [ "{",
                  "\"apiBaseUrl\": null,",
                  "\"nixUserIsTrusted\": null,",
                  "\"concurrentTasks\": null,",
                  "\"baseDirectory\": null,",
                  "\"staticSecretsDirectory\": null,",
                  "\"workDirectory\": null,",
                  "\"clusterJoinTokenPath\": null,",
                  "\"binaryCachesPath\": null,",
                  "\"secretsJsonPath\": null,",
                  "\"logLevel\": null,",
                  "\"labels\": null,",
                  "\"allowInsecureBuiltinFetchers\": null",
                  "}"
                ]
        Json.decode (forJson combiCodec) (toS $ encodeUtf8 inputs) `shouldBe` Right emptyConfig

      it "allows use of null in labels" $ do
        let inputs =
              T.unlines
                [ "{",
                  "\"labels\": {",
                  "  \"version\": null",
                  "}",
                  "}"
                ]
        Json.decode (forJson combiCodec) (toS $ encodeUtf8 inputs)
          `shouldBe` Right
            emptyConfig
              { labels = Just (M.singleton "version" A.Null)
              }

emptyConfig :: Config 'Input
emptyConfig =
  Config
    { herculesApiBaseURL = Nothing,
      nixUserIsTrusted = Nothing,
      concurrentTasks = Nothing,
      baseDirectory = Nothing,
      staticSecretsDirectory = Nothing,
      workDirectory = Nothing,
      clusterJoinTokenPath = Nothing,
      binaryCachesPath = Nothing,
      secretsJsonPath = Nothing,
      logLevel = Nothing,
      nixVerbosity = Nothing,
      labels = Nothing,
      allowInsecureBuiltinFetchers = Nothing,
      remotePlatformsWithSameFeatures = Nothing,
      effectMountables = mempty,
      nixSettings = mempty
    }
