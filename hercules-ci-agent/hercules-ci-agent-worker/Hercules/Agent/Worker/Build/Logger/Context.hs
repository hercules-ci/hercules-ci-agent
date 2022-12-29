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

import qualified Data.Map as M
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
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
        C.TypeName "HerculesLoggerEntry" =: [t|HerculesLoggerEntry|]
          <> C.TypeName "LogEntryQueue" =: [t|LogEntryQueue|]
          <> C.TypeName "LoggerFields" =: [t|Fields|]
    }
