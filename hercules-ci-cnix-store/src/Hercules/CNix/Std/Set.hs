{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | @std::set@
module Hercules.CNix.Std.Set
  ( stdSetCtx,
    instanceStdSet,
    instanceStdSetCopyable,
    CStdSet,
    StdSet (StdSet),
    Hercules.CNix.Std.Set.new,
    size,
    toSet,
    fromList,
    fromListP,
    fromListFP,
    Hercules.CNix.Std.Set.toList,
    insert,
    insertP,
    insertFP,
    toListFP,
  )
where

import Control.Exception (mask_)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (for_)
import qualified Data.Set as S
import Data.Traversable (for)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign
  ( ForeignPtr,
    FunPtr,
    Ptr,
    Storable,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.C (CSize)
import Hercules.CNix.Encapsulation (HasEncapsulation (..))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (Dec, Q)
import Prelude

data CStdSet a

stdSetCtx :: C.Context
stdSetCtx = C.cppCtx `mappend` C.cppTypePairs [("std::set", [t|CStdSet|])]

newtype StdSet a = StdSet (ForeignPtr (CStdSet a))

instance HasStdSet a => HasEncapsulation (CStdSet a) (StdSet a) where
  moveToForeignPtrWrapper = fmap StdSet . newForeignPtr cDelete

class HasStdSet a where
  cNew :: IO (Ptr (CStdSet a))
  cDelete :: FunPtr (Ptr (CStdSet a) -> IO ())
  cSize :: Ptr (CStdSet a) -> IO CSize
  cInsertByPtr :: Ptr a -> Ptr (CStdSet a) -> IO ()
  cCopies :: Ptr (CStdSet a) -> Ptr (Ptr a) -> IO ()

class HasStdSet a => HasStdSetCopyable a where
  cCopyTo :: Ptr (CStdSet a) -> Ptr a -> IO ()
  cInsert :: a -> Ptr (CStdSet a) -> IO ()

-- | Helper for defining templated instances
roll :: String -> Q [Dec] -> Q [Dec]
roll cType d =
  concat
    <$> sequence
      [ C.include "<set>",
        C.include "<algorithm>",
        C.substitute
          [ ("T", const cType),
            ("SET", \var -> "$(std::set<" ++ cType ++ ">* " ++ var ++ ")")
          ]
          d
      ]

instanceStdSet :: String -> DecsQ
instanceStdSet cType =
  roll
    cType
    [d|
      instance HasStdSet $(C.getHaskellType False cType) where
        cNew = [CU.exp| std::set<@T()>* { new std::set<@T()>() } |]
        cDelete = [C.funPtr| void deleteStdSet(std::set<@T()>* set) { delete set; } |]
        cSize set = [CU.exp| size_t { @SET(set)->size() } |]
        cInsertByPtr ptr set = [CU.exp| void { @SET(set)->insert(*$(@T() *ptr)) } |]
        cCopies set dstPtr =
          [CU.block| void {
            const std::set<@T()>& set = *@SET(set);
            @T()** aim = $(@T()** dstPtr);
            for (auto item : set) {
              *aim = new @T()(item);
              aim++;
            }
          }|]
      |]

instanceStdSetCopyable :: String -> DecsQ
instanceStdSetCopyable cType =
  roll
    cType
    [d|
      instance HasStdSetCopyable $(C.getHaskellType False cType) where
        cCopyTo set dstPtr =
          [CU.block| void {
            const std::set<@T()>* set = @SET(set);
            std::copy(set->begin(), set->end(), $(@T()* dstPtr));
            } |]
        cInsert value set =
          [CU.exp| void { @SET(set)->insert($(@T() value)) }
        |]
      |]

new :: forall a. HasStdSet a => IO (StdSet a)
new = mask_ $ do
  moveToForeignPtrWrapper =<< cNew @a

size :: HasStdSet a => StdSet a -> IO Int
size (StdSet fptr) = fromIntegral <$> withForeignPtr fptr cSize

fromList :: HasStdSetCopyable a => [a] -> IO (StdSet a)
fromList as = do
  set <- new
  for_ as $ insert set
  pure set

fromListP :: HasStdSet a => [Ptr a] -> IO (StdSet a)
fromListP as = do
  set <- new
  for_ as $ insertP set
  pure set

fromListFP :: (Coercible a' (ForeignPtr a), HasStdSet a) => [a'] -> IO (StdSet a)
fromListFP as = do
  set <- new
  for_ as $ insertFP set
  pure set

toSet :: (HasStdSetCopyable a, Storable a, Ord a) => StdSet a -> IO (S.Set a)
toSet stdSet = do
  S.fromList . VS.toList <$> toVector stdSet

toVector :: (HasStdSetCopyable a, Storable a) => StdSet a -> IO (VS.Vector a)
toVector stdSet@(StdSet stdSetFPtr) = do
  vecSize <- size stdSet
  hsVec <- VSM.new vecSize
  withForeignPtr stdSetFPtr $ \stdSetPtr ->
    VSM.unsafeWith hsVec $ \hsVecPtr ->
      cCopyTo stdSetPtr hsVecPtr
  VS.unsafeFreeze hsVec

toList :: (HasStdSetCopyable a, Storable a) => StdSet a -> IO [a]
toList vec = VS.toList <$> toVector vec

toVectorP :: (HasStdSet a) => StdSet a -> IO (VS.Vector (Ptr a))
toVectorP stdSet@(StdSet stdSetFPtr) = do
  vecSize <- size stdSet
  hsVec <- VSM.new vecSize
  withForeignPtr stdSetFPtr $ \stdSetPtr ->
    VSM.unsafeWith hsVec $ \hsVecPtr ->
      cCopies stdSetPtr hsVecPtr
  VS.unsafeFreeze hsVec

toListP :: (HasStdSet a) => StdSet a -> IO [Ptr a]
toListP vec = VS.toList <$> toVectorP vec

toListFP :: (HasStdSet a, HasEncapsulation a b) => StdSet a -> IO [b]
toListFP vec = mask_ $ do
  ptrs <- toListP vec
  for ptrs moveToForeignPtrWrapper

insert :: HasStdSetCopyable a => StdSet a -> a -> IO ()
insert (StdSet fptr) value = withForeignPtr fptr (cInsert value)

insertP :: HasStdSet a => StdSet a -> Ptr a -> IO ()
insertP (StdSet fptr) ptr = withForeignPtr fptr (cInsertByPtr ptr)

insertFP :: (Coercible a' (ForeignPtr a), HasStdSet a) => StdSet a -> a' -> IO ()
insertFP (StdSet fptr) vfptr =
  withForeignPtr fptr $ \setPtr ->
    withForeignPtr (coerce vfptr) (\valPtr -> cInsertByPtr valPtr setPtr)
