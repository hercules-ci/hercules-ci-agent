{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hercules.Agent.Worker.Build.Logger.Context
  ( Fields,
    HerculesLoggerEntry,
    LogEntryQueue,
    context,
  )
where

import Data.Map qualified as M
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as C
import Language.C.Types qualified as C
import Protolude

data HerculesLoggerEntry

data Fields

data LogEntryQueue

context :: C.Context
context =
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> loggerContext

(=:) :: k -> a -> Map k a
(=:) = M.singleton

loggerContext :: C.Context
loggerContext =
  mempty
    { C.ctxTypesTable =
        C.TypeName "HerculesLoggerEntry"
          =: [t|HerculesLoggerEntry|]
          <> C.TypeName "LogEntryQueue"
          =: [t|LogEntryQueue|]
          <> C.TypeName "LoggerFields"
          =: [t|Fields|]
    }
