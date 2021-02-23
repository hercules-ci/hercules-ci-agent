{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Store.Context
  ( module Hercules.CNix.Store.Context,
    NixStore,
  )
where

import Cachix.Client.Store.Context (NixStore)
import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.Map as M
import qualified Foreign.C.String
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

data Ref a

data Strings

data Derivation

data StringsIterator

data DerivationOutputsIterator

data DerivationInputsIterator

data StringPairsIterator

data StringPairs

data SecretKey

context :: C.Context
context =
  C.cppCtx <> C.fptrCtx
    <> C.bsCtx
      { C.ctxTypesTable =
          M.singleton (C.TypeName "refStore") [t|Ref NixStore|]
            <> M.singleton (C.TypeName "Strings") [t|Strings|]
            <> M.singleton (C.TypeName "StringsIterator") [t|StringsIterator|]
            <> M.singleton (C.TypeName "StringPairs") [t|StringPairs|]
            <> M.singleton (C.TypeName "StringPairsIterator") [t|StringPairsIterator|]
            <> M.singleton (C.TypeName "Derivation") [t|Derivation|]
            <> M.singleton (C.TypeName "DerivationOutputsIterator") [t|DerivationOutputsIterator|]
            <> M.singleton (C.TypeName "DerivationInputsIterator") [t|DerivationInputsIterator|]
            <> M.singleton (C.TypeName "SecretKey") [t|SecretKey|]
      }

unsafeMallocBS :: MonadIO m => IO Foreign.C.String.CString -> m ByteString
unsafeMallocBS m = liftIO (unsafePackMallocCString =<< m)
