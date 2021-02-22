{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix.Internal.HerculesStore.Context where

import CNix.Eval.Context (Ref)
import qualified Data.Map as M
import Hercules.Agent.StoreFFI (ExceptionPtr)
import qualified Hercules.CNix.Store.Context as Store
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

data HerculesStore

context :: C.Context
context =
  C.cppCtx <> C.fptrCtx
    <> C.bsCtx
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
