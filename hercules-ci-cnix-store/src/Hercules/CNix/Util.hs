{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Functions calling Nix's libutil
module Hercules.CNix.Util
  ( setInterruptThrown,
    triggerInterrupt,
  )
where

import Hercules.CNix.Store.Context
  ( context,
  )
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
import Prelude ()

C.context context

C.include "<nix/config.h>"

C.include "<nix/affinity.hh>"

C.include "<nix/util.hh>"

C.using "namespace nix"

setInterruptThrown :: IO ()
setInterruptThrown =
  [C.throwBlock| void {
    nix::setInterruptThrown();
  } |]

triggerInterrupt :: IO ()
triggerInterrupt =
  [C.throwBlock| void {
    nix::triggerInterrupt();
  } |]
