{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Command.Eval where

import           Protolude
import           Data.Binary

data Eval = Eval
            { cwd :: FilePath
            , file :: Text
            , autoArguments :: Map Text Arg
            }
          deriving (Generic, Binary, Show, Eq)

data Arg = LiteralArg ByteString
         | ExprArg ByteString
          deriving (Generic, Binary, Show, Eq)
