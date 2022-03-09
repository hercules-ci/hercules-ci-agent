{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Hercules.Agent.NixFile.GitSource where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Hercules.CNix.Expr (ToRawValue, ViaJSON (ViaJSON))
import Protolude

data GitSource = GitSource
  { outPath :: Text,
    ref :: Text,
    rev :: Text,
    shortRev :: Text,
    branch :: Maybe Text,
    tag :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
  deriving (ToRawValue) via (ViaJSON GitSource)

fromRefRevPath :: Text -> Text -> Text -> GitSource
fromRefRevPath aRef aRev path =
  GitSource
    { outPath = path,
      ref = aRef,
      rev = aRev,
      shortRev = T.take 7 aRev,
      branch = T.stripPrefix "refs/heads/" aRef,
      tag = T.stripPrefix "refs/tags/" aRef
    }
