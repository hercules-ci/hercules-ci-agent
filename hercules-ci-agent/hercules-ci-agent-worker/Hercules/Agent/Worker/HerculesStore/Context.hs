{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hercules.Agent.Worker.HerculesStore.Context
  ( context,
    HerculesStore,
    ExceptionPtr,
  )
where

import qualified Data.Map as M
import Hercules.CNix.Expr.Context (Ref)
import Hercules.CNix.Std.Vector (stdVectorCtx)
import qualified Hercules.CNix.Store.Context as Store
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

data HerculesStore

data ExceptionPtr

context :: C.Context
context =
  C.cppCtx <> C.fptrCtx
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
        C.TypeName "refHerculesStore" =: [t|Ref HerculesStore|]
          <> C.TypeName "exception_ptr" =: [t|ExceptionPtr|]
    }
