{-# LANGUAGE ScopedTypeVariables #-}

module Data.Time.Extras where

import Control.Concurrent.Thread.Delay
import Data.Fixed
import Data.Fixed.Extras
import Data.Time
import Prelude

delayMicro :: Micro -> IO ()
delayMicro (MkFixed mus) = delay mus

delayNominalDiffTime :: NominalDiffTime -> IO ()
delayNominalDiffTime = delayMicro . toFixed ceiling
