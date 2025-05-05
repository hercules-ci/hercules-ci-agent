{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Functions calling Nix's libutil
module Hercules.CNix.Util
  ( setInterruptThrown,
    triggerInterrupt,
    installDefaultSigINTHandler,
    createInterruptCallback,
  )
where

import Hercules.CNix.Store.Context
  ( context,
  )
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude
import System.Mem.Weak (deRefWeak)
import System.Posix (Handler (Catch), installHandler, sigHUP, sigINT, sigTERM, sigUSR1)
import Prelude ()

C.context context

#if NIX_IS_AT_LEAST(2, 28, 0)

C.include "<nix/util/signals.hh>"

#else
C.include "<nix/config.h>"
C.include "<nix/util.hh>"
#  if NIX_IS_AT_LEAST(2,19,0)
C.include "<nix/signals.hh>"
#  endif
#endif

C.using "namespace nix"

setInterruptThrown :: IO ()
setInterruptThrown =
  [C.throwBlock| void {
    nix::setInterruptThrown();
  } |]

triggerInterrupt :: IO ()
triggerInterrupt =
#if NIX_IS_AT_LEAST(2,24,0)
  [C.throwBlock| void {
    nix::unix::triggerInterrupt();
  } |]
#else
  [C.throwBlock| void {
    nix::triggerInterrupt();
  } |]
#endif

installDefaultSigINTHandler :: IO ()
installDefaultSigINTHandler = do
  mainThread <- myThreadId
  weakId <- mkWeakThreadId mainThread
  let defaultHaskellHandler = do
        mt <- deRefWeak weakId
        for_ mt \t -> do
          throwTo t (toException UserInterrupt)

  -- Install Nix interrupter in Haskell
  _oldHandler <-
    for [sigINT, sigTERM, sigHUP] \sig ->
      installHandler
        sig
        ( Catch do
            triggerInterrupt
            defaultHaskellHandler
        )
        Nothing

  -- Install dummy SIGUSR1 handler for Nix interrupt signal propagation
  -- (installHandler uses process-wide sigprocmask, so this should apply to all
  -- capability threads, as required for Nix)
  _oldHandler <-
    installHandler
      sigUSR1
      ( -- Not Ignore, because we want to cause EINTR
        Catch pass
      )
      Nothing

  -- Install Haskell interrupter in Nix
  createInterruptCallback defaultHaskellHandler

createInterruptCallback :: IO () -> IO ()
createInterruptCallback onInterrupt = do
  onInterruptPtr <- mkCallback onInterrupt
  -- leaks onInterruptPtr
  [C.throwBlock| void {
    nix::createInterruptCallback($(void (*onInterruptPtr)()));
  } |]

#ifndef __GHCIDE__
foreign import ccall "wrapper"
  mkCallback :: IO () -> IO (FunPtr (IO ()))
#else
mkCallback :: IO () -> IO (FunPtr (IO ()))
mkCallback = panic "This is a stub to work around a ghcide issue. Please compile without -D__GHCIDE__"
#endif
