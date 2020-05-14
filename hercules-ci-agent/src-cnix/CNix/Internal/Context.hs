{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix.Internal.Context
  ( module CNix.Internal.Context,
    NixStore,
  )
where

import Cachix.Client.Store.Context (NixStore)
import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.Map as M
import qualified Foreign.C.String
import Hercules.Agent.StoreFFI (ExceptionPtr)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

data EvalState

data Strings

data Ref a

data Bindings'

data Value'

data Attr'

data HerculesStore

data Derivation

data Fields

data HerculesLoggerEntry

data LogEntryQueue

data StringsIterator

data DerivationOutputsIterator

data DerivationInputsIterator

data StringPairsIterator

data StringPairs

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
            <> M.singleton (C.TypeName "StringsIterator") [t|StringsIterator|]
            <> M.singleton (C.TypeName "StringPairs") [t|StringPairs|]
            <> M.singleton (C.TypeName "StringPairsIterator") [t|StringPairsIterator|]
            <> M.singleton (C.TypeName "refHerculesStore") [t|Ref HerculesStore|]
            <> M.singleton (C.TypeName "Derivation") [t|Derivation|]
            <> M.singleton (C.TypeName "LoggerFields") [t|Fields|]
            <> M.singleton (C.TypeName "HerculesLoggerEntry") [t|HerculesLoggerEntry|]
            <> M.singleton (C.TypeName "LogEntryQueue") [t|LogEntryQueue|]
            <> M.singleton (C.TypeName "DerivationOutputsIterator") [t|DerivationOutputsIterator|]
            <> M.singleton (C.TypeName "DerivationInputsIterator") [t|DerivationInputsIterator|]
            <> M.singleton (C.TypeName "exception_ptr") [t|ExceptionPtr|]
      }

unsafeMallocBS :: MonadIO m => IO Foreign.C.String.CString -> m ByteString
unsafeMallocBS m = liftIO (unsafePackMallocCString =<< m)
