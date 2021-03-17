module Hercules.CNix.Encapsulation where

import Foreign (Ptr)
import Prelude

class HasEncapsulation a b where
  -- | Takes ownership of the pointer, freeing/finalizing the pointer when
  -- collectable.
  moveToForeignPtrWrapper :: Ptr a -> IO b
