{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hercules.CNix.Expr.Context
  ( context,
    evalContext,
    EvalState,
    Value',
    Attr',
    BindingsBuilder',
    ListBuilder',
    module Hercules.CNix.Store.Context,
    (=:),
  )
where

import qualified Data.Map as M
import Hercules.CNix.Store.Context hiding (context)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

data EvalState

data Value'

data Attr'

data BindingsBuilder'

data ListBuilder'

data ListViewType'

context :: C.Context
context =
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> evalContext

(=:) :: k -> a -> Map k a
(=:) = M.singleton

evalContext :: C.Context
evalContext =
  mempty
    { C.ctxTypesTable =
        C.TypeName "EvalState"
          =: [t|EvalState|]
          <> C.TypeName "Value"
          =: [t|Value'|]
          <> C.TypeName "Attr"
          =: [t|Attr'|]
          <> C.TypeName "BindingsBuilder"
          =: [t|BindingsBuilder'|]
          <> C.TypeName "ListBuilder"
          =: [t|ListBuilder'|]
          <> C.TypeName "ListViewType"
          =: [t|ListViewType'|]
    }
