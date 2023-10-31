{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hercules.Agent.Worker.HerculesStore.Context
  ( context,
    HerculesStore,
    ExceptionPtr,
  )
where

import Data.Map qualified as M
import Hercules.CNix.Expr.Context (Ref)
import Hercules.CNix.Std.Vector (stdVectorCtx)
import Hercules.CNix.Store.Context qualified as Store
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as C
import Language.C.Types qualified as C
import Protolude

data HerculesStore

data ExceptionPtr

context :: C.Context
context =
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> stdVectorCtx
    <> Store.context
    <> herculesStoreContext

(=:) :: k -> a -> Map k a
(=:) = M.singleton

herculesStoreContext :: C.Context
herculesStoreContext =
  mempty
    { C.ctxTypesTable =
        C.TypeName "refHerculesStore"
          =: [t|Ref HerculesStore|]
          <> C.TypeName "exception_ptr"
          =: [t|ExceptionPtr|]
    }
