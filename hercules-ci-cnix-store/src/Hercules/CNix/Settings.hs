{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Settings where

import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.Set as S
import Hercules.CNix.Encapsulation (moveToForeignPtrWrapper)
import qualified Hercules.CNix.Std.Set as Std.Set
import qualified Hercules.CNix.Std.String as Std.String
import Hercules.CNix.Std.String.Instances ()
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store.Context (context)
import qualified Language.C.Inline.Cpp as C
import Protolude hiding (evalState, throwIO)

C.context
  ( context
      <> Std.Set.stdSetCtx
      <> Std.String.stdStringCtx
      <> Std.Vector.stdVectorCtx
  )

C.include "<cstring>"
C.include "<nix/config.h>"
C.include "<nix/globals.hh>"
C.include "<set>"
C.include "<string>"

byteStringSet :: IO (Ptr (Std.Set.CStdSet Std.String.CStdString)) -> IO (Set ByteString)
byteStringSet x =
  x
    >>= moveToForeignPtrWrapper
    >>= Std.Set.toListFP
    >>= traverse Std.String.copyToByteString
    <&> S.fromList

byteStringList :: IO (Ptr (Std.Vector.CStdVector Std.String.CStdString)) -> IO [ByteString]
byteStringList x =
  x
    >>= moveToForeignPtrWrapper
    >>= Std.Vector.toListFP
    >>= traverse Std.String.copyToByteString

getExtraPlatforms :: IO (Set ByteString)
getExtraPlatforms =
  byteStringSet
    [C.block| std::set<std::string>*{
      return new nix::StringSet(nix::settings.extraPlatforms.get());
    }|]

getSystem :: IO ByteString
getSystem =
  unsafePackMallocCString
    =<< [C.exp| const char *{
      strdup(nix::settings.thisSystem.get().c_str())
    }|]

getSystemFeatures :: IO (Set ByteString)
getSystemFeatures =
  byteStringSet
    [C.block| std::set<std::string>*{
      return new nix::StringSet(nix::settings.systemFeatures.get());
    }|]

getSubstituters :: IO [ByteString]
getSubstituters =
  byteStringList
    [C.block| std::vector<std::string>*{
      auto r = new std::vector<std::string>();
      for (auto i : nix::settings.substituters.get())
        r->push_back(i);
      return r;
    }|]

getTrustedPublicKeys :: IO [ByteString]
getTrustedPublicKeys =
  byteStringList
    [C.block| std::vector<std::string>*{
      auto r = new std::vector<std::string>();
      for (auto i : nix::settings.trustedPublicKeys.get())
        r->push_back(i);
      return r;
    }|]

getNarinfoCacheNegativeTtl :: IO Word64
getNarinfoCacheNegativeTtl =
  [C.exp| uint64_t{
    nix::settings.ttlNegativeNarInfoCache.get()
  }|]

getNetrcFile :: IO ByteString
getNetrcFile =
  unsafePackMallocCString
    =<< [C.exp| const char *{
      strdup(nix::settings.netrcFile.get().c_str())
    }|]
