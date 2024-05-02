{-# LANGUAGE DeriveAnyClass #-}

-- This module pins down the details of week days in the API. It's boilerplate-y
-- but that makes it robust.

-- | A day of week type. Integer representations are not used in the API, but
-- are provided for convenience and will not change.
module Hercules.API.DayOfWeek
  ( DayOfWeek (..),

    -- * Conversions for @time@ package
    toTime,
    fromTime,

    -- * Optional, stable, opinionated integer conversions
    toNum,
    fromNumMaybe,
    fromNum,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromJust)
import qualified Data.OpenApi as O3
import Data.Swagger (ToSchema)
import qualified Data.Time
import GHC.Generics (Generic)
import Prelude (Eq, Integral, Maybe (..), Num (..), Show, mod)

-- | Day of week representation used in the API.
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

-- | Conversion to @time@ package representation.
toTime :: DayOfWeek -> Data.Time.DayOfWeek
toTime Mon = Data.Time.Monday
toTime Tue = Data.Time.Tuesday
toTime Wed = Data.Time.Wednesday
toTime Thu = Data.Time.Thursday
toTime Fri = Data.Time.Friday
toTime Sat = Data.Time.Saturday
toTime Sun = Data.Time.Sunday

-- | Conversion from @time@ package representation.
fromTime :: Data.Time.DayOfWeek -> DayOfWeek
fromTime Data.Time.Monday = Mon
fromTime Data.Time.Tuesday = Tue
fromTime Data.Time.Wednesday = Wed
fromTime Data.Time.Thursday = Thu
fromTime Data.Time.Friday = Fri
fromTime Data.Time.Saturday = Sat
fromTime Data.Time.Sunday = Sun

-- | Conversion to integers where 'Mon' -> 1, 'Sun' -> 7
toNum :: (Num n) => DayOfWeek -> n
toNum Mon = 1
toNum Tue = 2
toNum Wed = 3
toNum Thu = 4
toNum Fri = 5
toNum Sat = 6
toNum Sun = 7

-- | Conversion from integers where @1 -> Just 'Mon'@, @7 -> Just 'Sun'@, @outside of 1..7 -> Nothing@
fromNumMaybe :: (Num n, Eq n) => n -> Maybe DayOfWeek
fromNumMaybe 1 = Just Mon
fromNumMaybe 2 = Just Tue
fromNumMaybe 3 = Just Wed
fromNumMaybe 4 = Just Thu
fromNumMaybe 5 = Just Fri
fromNumMaybe 6 = Just Sat
fromNumMaybe 7 = Just Sun
fromNumMaybe _ = Nothing

-- | Conversion from integers where @{...,-7,0,7,...} -> Just 'Sun'@, @{...,-6,1,8,...} -> Just 'Mon'@, etc.
--
-- Requires a sensible implementation of the 'Integral' 'mod' method that returns non-negative numbers.
-- 'Integer', 'Int', 'Int8', etc are ok. So is the 'Word' family of types, though beware of 'minBound' overflows.
fromNum :: (Integral n) => n -> DayOfWeek
fromNum n = fromJust (fromNumMaybe (1 + (n - 1) `mod` 7))
