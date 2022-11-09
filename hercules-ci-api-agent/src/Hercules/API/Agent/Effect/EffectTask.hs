{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Effect.EffectTask where

import Control.Applicative
import Control.Lens (at, (%~))
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Hercules.API.Prelude
import Hercules.API.Task (Task)

data EffectTask = EffectTask
  { id :: Id (Task EffectTask),
    derivationPath :: Text,
    logToken :: Text,
    inputDerivationOutputPaths :: [Text],
    token :: Text,
    serverSecrets :: Map Text (Map Text A.Value),
    projectId :: Id "project",
    projectPath :: Text,
    siteName :: Text,
    ownerName :: Text,
    repoName :: Text,
    ref :: Text,
    isDefaultBranch :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON)

instance FromJSON EffectTask where
  parseJSON v = A.genericParseJSON A.defaultOptions (fixup v)
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "serverSecrets" %~ (<|> Just (A.object []))
