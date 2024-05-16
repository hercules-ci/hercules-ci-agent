-- | Memory management utilities.
module Hercules.CNix.Memory
  ( -- * Free after use
    Delete (..),
    withDelete,

    -- * Free on GC
    Finalizer (..),
    toForeignPtr,

    -- * Nullable pointers
    forNonNull,
    traverseNonNull,
  )
where

import Foreign (FinalizerPtr, ForeignPtr, newForeignPtr, nullPtr)
import Protolude

-- | Types whose memory / resources can be freed in a consistent way.
class Delete a where
  delete :: Ptr a -> IO ()

-- | Obtain a pointer to a resource and run an action with it.
withDelete :: (Delete a) => IO (Ptr a) -> (Ptr a -> IO b) -> IO b
withDelete make = bracket make delete

-- | Like 'Delete', but the design of finalizers favors that we implement it
-- by means of a function pointer instead of a Haskell function. That way, it
-- can be run during GC, without the need for a separate thread and such.
--
-- NOTE: This should always return a CAF, to avoid repeated allocation,
-- initialization, etc.
--
-- Example:
--
-- @
-- instance Finalizer CStdString where
--   finalizer = finalize
--
-- finalize :: FinalizerPtr CStdString
-- {-# NOINLINE finalize #-}
-- finalize =
--   unsafePerformIO
--     [C.exp|
--       void (*)(std::string *) {
--         [](std::string *v) {
--           delete v;
--         }
--       }
--     |]
-- @
class Finalizer a where
  finalizer :: FinalizerPtr a

-- | Construct a 'ForeignPtr' using 'finalizer'.
-- This takes ownership of the pointer, so it must only be called once per pointer.
toForeignPtr :: (Finalizer a) => Ptr a -> IO (ForeignPtr a)
toForeignPtr ptr = newForeignPtr finalizer ptr

-- Pointer utilities

-- | Turn an action on pointer into an action that returns 'Nothing' iff the pointer is 'nullPtr'.
--
-- Same as 'flip' 'forNonNull'.
traverseNonNull :: (Applicative m) => (Ptr a -> m b) -> Ptr a -> m (Maybe b)
traverseNonNull f p = if p == nullPtr then pure Nothing else Just <$> f p

-- | Run an action with a pointer, if it is not 'nullPtr'.
--
-- Same as 'flip' 'traverseNonNull'.
forNonNull :: (Applicative m) => Ptr a -> (Ptr a -> m b) -> m (Maybe b)
forNonNull = flip traverseNonNull
