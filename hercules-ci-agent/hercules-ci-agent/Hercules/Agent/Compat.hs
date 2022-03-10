{-# LANGUAGE CPP #-}
module Hercules.Agent.Compat where

import qualified Katip as K

#if MIN_VERSION_katip(0,8,0)
katipLevel :: K.Severity -> K.PermitFunc
katipLevel a =
  K.permitItem a
#else
katipLevel x = x
#endif