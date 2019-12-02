{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix.Internal.Context where

import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.Map as M
import qualified Foreign.C.String
import Hercules.Agent.StoreFFI (ExceptionPtr)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

data NixStore

data EvalState

data Strings

data Ref a

data Bindings'

data Value'

data Attr'

data HerculesStore

data Derivation

context :: C.Context
context =
  C.cppCtx <> C.fptrCtx
    <> C.bsCtx
      { C.ctxTypesTable =
          M.singleton (C.TypeName "refStore") [t|Ref NixStore|]
            <> M.singleton (C.TypeName "EvalState") [t|EvalState|]
            <> M.singleton (C.TypeName "Bindings") [t|Bindings'|]
            <> M.singleton (C.TypeName "Value") [t|Value'|]
            <> M.singleton (C.TypeName "Attr") [t|Attr'|]
            <> M.singleton (C.TypeName "Strings") [t|Strings|]
            <> M.singleton (C.TypeName "refHerculesStore") [t|Ref HerculesStore|]
            <> M.singleton (C.TypeName "Derivation") [t|Derivation|]
            <> M.singleton (C.TypeName "exception_ptr") [t|ExceptionPtr|]
      }

unsafeMallocBS :: MonadIO m => IO Foreign.C.String.CString -> m ByteString
unsafeMallocBS m = liftIO (unsafePackMallocCString =<< m)
