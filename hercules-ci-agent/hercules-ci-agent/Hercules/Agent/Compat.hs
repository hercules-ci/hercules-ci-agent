{-# LANGUAGE CPP #-}

module Hercules.Agent.Compat (katipLevel) where

import qualified Katip as K
import Protolude

katipLevel :: K.Severity -> K.PermitFunc
#if MIN_VERSION_katip(0,8,0)
katipLevel a =
  K.permitItem a
#else
katipLevel x = x
#endif

{- eta expansion required for type checking -}
{-# ANN katipLevel ("HLint: ignore Eta reduce" :: Text) #-}
