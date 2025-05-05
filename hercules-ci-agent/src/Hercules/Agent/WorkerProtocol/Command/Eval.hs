{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Eval where

import Data.Binary
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.ImmutableGitInput (ImmutableGitInput)
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.WorkerProtocol.Orphans ()
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON)
import Protolude

data Eval = Eval
  { cwd :: FilePath,
    file :: Text,
    autoArguments :: Map Text ByteString,
    -- TODO: Also set at worker start, so remove this here to avoid ambiguity?
    extraNixOptions :: [(Text, Text)],
    gitSource :: ViaJSON GitSource,
    srcInput :: Maybe (ViaJSON ImmutableGitInput),
    apiBaseUrl :: Text,
    selector :: ViaJSON EvaluateTask.Selector,
    isFlakeJob :: Bool,
    ciSystems :: Maybe (Map Text ()),
    allowInsecureBuiltinFetchers :: Bool,
    allowedPaths :: [ByteString]
  }
  deriving (Generic, Binary, Show, Eq)
