{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Hercules.Agent.NixFile.GitSource where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Hercules.CNix.Expr (ToRawValue, ViaJSON (ViaJSON))
import Protolude

data GitSource = GitSource
  { outPath :: Text,
    ref :: Text,
    rev :: Text,
    shortRev :: Text,
    branch :: Maybe Text,
    tag :: Maybe Text,
    remoteHttpUrl :: Maybe Text,
    remoteSshUrl :: Maybe Text,
    webUrl :: Maybe Text,
    forgeType :: Maybe Text,
    owner :: Maybe Text,
    name :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
  deriving (ToRawValue) via (ViaJSON GitSource)

shortRevFromRev :: Text -> Text
shortRevFromRev = T.take 7

branchFromRef :: Text -> Maybe Text
branchFromRef = T.stripPrefix "refs/heads/"

tagFromRef :: Text -> Maybe Text
tagFromRef = T.stripPrefix "refs/tags/"
