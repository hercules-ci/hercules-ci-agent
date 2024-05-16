{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hercules.CNix.Encapsulation
  ( HasEncapsulation (..),
    nullableMoveToForeignPtrWrapper,
  )
where

import Data.Coerce (Coercible, coerce)
import Foreign (ForeignPtr, Ptr, newForeignPtr, nullPtr)
import Hercules.CNix.Memory (Finalizer, finalizer)
import Prelude

class HasEncapsulation a b | b -> a where
  -- | Takes ownership of the pointer, freeing/finalizing the pointer when
  -- collectable.
  moveToForeignPtrWrapper :: Ptr a -> IO b
  default moveToForeignPtrWrapper :: (Finalizer a, Coercible b (ForeignPtr a)) => Ptr a -> IO b
  moveToForeignPtrWrapper = coerce <$> newForeignPtr finalizer

nullableMoveToForeignPtrWrapper :: (HasEncapsulation a b) => Ptr a -> IO (Maybe b)
nullableMoveToForeignPtrWrapper rawPtr | rawPtr == nullPtr = pure Nothing
nullableMoveToForeignPtrWrapper rawPtr = Just <$> moveToForeignPtrWrapper rawPtr
