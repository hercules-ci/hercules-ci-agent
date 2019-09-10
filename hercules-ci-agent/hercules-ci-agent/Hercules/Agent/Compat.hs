{-# LANGUAGE CPP #-}
module Hercules.Agent.Compat where

import qualified Katip as K
import Protolude 

#if MIN_VERSION_katip(0,8,0)
katipLevel :: K.Severity -> K.PermitFunc
katipLevel =
  K.permitItem
#else
katipLevel =
  identity
#endif