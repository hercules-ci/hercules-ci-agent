{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Command.Eval where

import           Protolude
import           Data.Binary

data Eval = Eval
            { cwd :: FilePath
            , file :: Text
            , autoArguments :: Map Text Arg
            , extraNixOptions :: [(Text, Text)]
              -- ^ NB currently the options will leak from one evaluation to
              --   the next if you're running them in the same worker!
              --   (as of now, we use one worker process per evaluation)
            }
          deriving (Generic, Binary, Show, Eq)

data Arg = LiteralArg ByteString
         | ExprArg ByteString
          deriving (Generic, Binary, Show, Eq)
