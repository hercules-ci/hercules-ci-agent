{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Eval where

import Data.Binary
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import Hercules.API.Agent.Evaluate.ImmutableGitInput (ImmutableGitInput)
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.WorkerProtocol.LogSettings
import Hercules.Agent.WorkerProtocol.Orphans ()
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON)
import Protolude

data Eval = Eval
  { cwd :: FilePath,
    file :: Text,
    autoArguments :: Map Text Arg,
    -- | NB currently the options will leak from one evaluation to
    --   the next if you're running them in the same worker!
    --   (as of now, we use one worker process per evaluation)
    extraNixOptions :: [(Text, Text)],
    gitSource :: ViaJSON GitSource,
    srcInput :: Maybe (ViaJSON ImmutableGitInput),
    apiBaseUrl :: Text,
    logSettings :: LogSettings,
    selector :: ViaJSON EvaluateTask.Selector,
    isFlakeJob :: Bool,
    ciSystems :: Maybe (Map Text ()),
    allowInsecureBuiltinFetchers :: Bool,
    allowedPaths :: [ByteString]
  }
  deriving (Generic, Binary, Show, Eq)

data Arg
  = LiteralArg ByteString
  | ExprArg ByteString
  deriving (Generic, Binary, Show, Eq)
