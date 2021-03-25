module Hercules.CNix.Encapsulation
  ( HasEncapsulation (..),
    nullableMoveToForeignPtrWrapper,
  )
where

import Foreign (Ptr, nullPtr)
import Prelude

class HasEncapsulation a b where
  -- | Takes ownership of the pointer, freeing/finalizing the pointer when
  -- collectable.
  moveToForeignPtrWrapper :: Ptr a -> IO b

nullableMoveToForeignPtrWrapper :: HasEncapsulation a b => Ptr a -> IO (Maybe b)
nullableMoveToForeignPtrWrapper rawPtr | rawPtr == nullPtr = pure Nothing
nullableMoveToForeignPtrWrapper rawPtr = Just <$> moveToForeignPtrWrapper rawPtr
