{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Eval where

import Data.Binary
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.WorkerProtocol.Event (ViaJSON)
import Hercules.Agent.WorkerProtocol.LogSettings
import Hercules.Agent.WorkerProtocol.Orphans ()
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
    apiBaseUrl :: Text,
    logSettings :: LogSettings,
    selector :: ViaJSON EvaluateTask.Selector
  }
  deriving (Generic, Binary, Show, Eq)

data Arg
  = LiteralArg ByteString
  | ExprArg ByteString
  deriving (Generic, Binary, Show, Eq)
