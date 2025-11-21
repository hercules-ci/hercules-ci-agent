{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hercules.CNix.Store.Context where

import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.Map as M
import qualified Foreign.C.String
-- Import C API Store type for inline-c context
import qualified Hercules.CNix.Nix.API.Store as CAPI
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude

-- | A C++ @std::string@
data StdString

-- | A Nix @ref@, to be used in phantom types.
data Ref a

-- | A Nix @Strings@ aka @std::list<std::string>@
data Strings

data NixStore

data ValidPathInfo

data Derivation

data StringsIterator

data DerivationOutputsIterator

data DerivationInputsIterator

data StringPairsIterator

data StringPairs

data SecretKey

data NixStorePath

data NixStorePathWithOutputs

data CDerivation

context :: C.Context
context =
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> mempty
      { C.ctxTypesTable =
          M.singleton (C.TypeName "nix_store") [t|CAPI.Store|]
            <> M.singleton (C.TypeName "refStore") [t|Ref NixStore|]
            <> M.singleton (C.TypeName "nix::StorePath") [t|NixStorePath|]
            <> M.singleton (C.TypeName "nix::StorePathWithOutputs") [t|NixStorePathWithOutputs|]
            <> M.singleton (C.TypeName "refValidPathInfo") [t|Ref ValidPathInfo|]
            <> M.singleton (C.TypeName "Strings") [t|Strings|]
            <> M.singleton (C.TypeName "StringsIterator") [t|StringsIterator|]
            <> M.singleton (C.TypeName "StringPairs") [t|StringPairs|]
            <> M.singleton (C.TypeName "StringPairsIterator") [t|StringPairsIterator|]
            <> M.singleton (C.TypeName "Derivation") [t|Derivation|]
            <> M.singleton (C.TypeName "DerivationOutputsIterator") [t|DerivationOutputsIterator|]
            <> M.singleton (C.TypeName "DerivationInputsIterator") [t|DerivationInputsIterator|]
            <> M.singleton (C.TypeName "SecretKey") [t|SecretKey|]
      }

unsafeMallocBS :: (MonadIO m) => IO Foreign.C.String.CString -> m ByteString
unsafeMallocBS m = liftIO (unsafePackMallocCString =<< m)
{-# DEPRECATED unsafeMallocBS "Use unsafePackMallocCString" #-}
