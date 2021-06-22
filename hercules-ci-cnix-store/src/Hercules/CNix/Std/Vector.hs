{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | @std::vector@
--
-- Original author @chpatrick https://github.com/fpco/inline-c/blob/1ba35141e330981fef0457a1619701b8acc32f0b/inline-c-cpp/test/StdVector.hs
module Hercules.CNix.Std.Vector
  ( stdVectorCtx,
    instanceStdVector,
    instanceStdVectorCopyable,
    CStdVector,
    StdVector (StdVector),
    Hercules.CNix.Std.Vector.new,
    size,
    toVector,
    toVectorP,
    toListP,
    toListFP,
    Hercules.CNix.Std.Vector.toList,
    Hercules.CNix.Std.Vector.fromList,
    fromListFP,
    pushBack,
    pushBackP,
    pushBackFP,
  )
where

import Control.Exception (mask_)
import Data.Coerce (Coercible, coerce)
import Data.Foldable
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign
import Foreign.C
import Hercules.CNix.Encapsulation
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Haskell.TH
import Prelude

data CStdVector a

stdVectorCtx :: C.Context
stdVectorCtx = C.cppCtx `mappend` C.cppTypePairs [("std::vector", [t|CStdVector|])]

newtype StdVector a = StdVector (ForeignPtr (CStdVector a))

instance HasStdVector a => HasEncapsulation (CStdVector a) (StdVector a) where
  moveToForeignPtrWrapper x = StdVector <$> newForeignPtr cDelete x

class HasStdVector a where
  cNew :: IO (Ptr (CStdVector a))
  cDelete :: FunPtr (Ptr (CStdVector a) -> IO ())
  cSize :: Ptr (CStdVector a) -> IO CSize
  cCopies :: Ptr (CStdVector a) -> Ptr (Ptr a) -> IO ()
  cPushBackByPtr :: Ptr a -> Ptr (CStdVector a) -> IO ()

class HasStdVector a => HasStdVectorCopyable a where
  cCopyTo :: Ptr (CStdVector a) -> Ptr a -> IO ()
  cPushBack :: a -> Ptr (CStdVector a) -> IO ()

-- | Helper for defining templated instances
roll :: String -> Q [Dec] -> Q [Dec]
roll cType d =
  concat
    <$> sequence
      [ C.include "<vector>",
        C.include "<algorithm>",
        C.substitute
          [ ("T", const cType),
            ("VEC", \var -> "$(std::vector<" ++ cType ++ ">* " ++ var ++ ")")
          ]
          d
      ]

instanceStdVector :: String -> DecsQ
instanceStdVector cType =
  roll
    cType
    [d|
      instance HasStdVector $(C.getHaskellType False cType) where
        cNew = [CU.exp| std::vector<@T()>* { new std::vector<@T()>() } |]
        cDelete = [C.funPtr| void deleteStdVector(std::vector<@T()>* vec) { delete vec; } |]
        cSize vec = [CU.exp| size_t { @VEC(vec)->size() } |]

        cCopies vec dstPtr =
          [CU.block| void {
          const std::vector<@T()>& vec = *@VEC(vec);
          @T()** aim = $(@T()** dstPtr);
          for (auto item : vec) {
            *aim = new @T()(item);
            aim++;
          }
        }|]
        cPushBackByPtr ptr vec = [CU.exp| void { @VEC(vec)->push_back(*$(@T() *ptr)) } |]
      |]

instanceStdVectorCopyable :: String -> DecsQ
instanceStdVectorCopyable cType =
  roll
    cType
    [d|
      instance HasStdVectorCopyable $(C.getHaskellType False cType) where
        cCopyTo vec dstPtr =
          [CU.block| void {
          const std::vector<@T()>* vec = @VEC(vec);
          std::copy(vec->begin(), vec->end(), $(@T()* dstPtr));
          } |]
        cPushBack value vec = [CU.exp| void { @VEC(vec)->push_back($(@T() value)) } |]
      |]

new :: forall a. HasStdVector a => IO (StdVector a)
new = mask_ $ do
  ptr <- cNew @a
  StdVector <$> newForeignPtr cDelete ptr

size :: HasStdVector a => StdVector a -> IO Int
size (StdVector fptr) = fromIntegral <$> withForeignPtr fptr cSize

toVector :: (HasStdVectorCopyable a, Storable a) => StdVector a -> IO (VS.Vector a)
toVector stdVec@(StdVector stdVecFPtr) = do
  vecSize <- size stdVec
  hsVec <- VSM.new vecSize
  withForeignPtr stdVecFPtr $ \stdVecPtr ->
    VSM.unsafeWith hsVec $ \hsVecPtr ->
      cCopyTo stdVecPtr hsVecPtr
  VS.unsafeFreeze hsVec

toVectorP :: HasStdVector a => StdVector a -> IO (VS.Vector (Ptr a))
toVectorP stdVec@(StdVector stdVecFPtr) = do
  vecSize <- size stdVec
  hsVec <- VSM.new vecSize
  withForeignPtr stdVecFPtr $ \stdVecPtr ->
    VSM.unsafeWith hsVec $ \hsVecPtr ->
      cCopies stdVecPtr hsVecPtr
  VS.unsafeFreeze hsVec

fromList :: HasStdVectorCopyable a => [a] -> IO (StdVector a)
fromList as = do
  vec <- Hercules.CNix.Std.Vector.new
  for_ as $ \a -> pushBack vec a
  pure vec

fromListFP :: (Coercible a' (ForeignPtr a), HasStdVector a) => [a'] -> IO (StdVector a)
fromListFP as = do
  vec <- Hercules.CNix.Std.Vector.new
  for_ as $ \a -> pushBackFP vec a
  pure vec

toList :: (HasStdVectorCopyable a, Storable a) => StdVector a -> IO [a]
toList vec = VS.toList <$> toVector vec

toListP :: (HasStdVector a) => StdVector a -> IO [Ptr a]
toListP vec = VS.toList <$> toVectorP vec

toListFP :: (HasEncapsulation a b, HasStdVector a) => StdVector a -> IO [b]
toListFP vec = traverse moveToForeignPtrWrapper =<< toListP vec

pushBack :: HasStdVectorCopyable a => StdVector a -> a -> IO ()
pushBack (StdVector fptr) value = withForeignPtr fptr (cPushBack value)

pushBackP :: HasStdVector a => StdVector a -> Ptr a -> IO ()
pushBackP (StdVector fptr) valueP = withForeignPtr fptr (cPushBackByPtr valueP)

pushBackFP :: (Coercible a' (ForeignPtr a), HasStdVector a) => StdVector a -> a' -> IO ()
pushBackFP vec vfptr = withForeignPtr (coerce vfptr) (pushBackP vec)
