{-# LANGUAGE ScopedTypeVariables #-}

module Data.Fixed.Extras where

import Data.Fixed
import Prelude

-- | Round to fixed precision using a rounding function.
--
-- >>> toFixed ceiling 0.12345 :: Milli
-- 0.124
toFixed :: forall res a. (Num a, HasResolution res) => (a -> Integer) -> a -> Fixed res
toFixed rounding a =
  let r = resolution out
      out = MkFixed $ rounding (a * fromIntegral r)
   in out
