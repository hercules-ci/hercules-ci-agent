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

C.include "signals.hxx"

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

-- | Install a signal handler that will put Nix into the interrupted state and
-- throws 'UserInterrupt' in the main thread (as is usual), assuming this
-- function is called from the main thread.
--
-- This installs a synchronous C signal handler that calls nix::triggerInterrupt()
-- immediately when signals are delivered, eliminating race conditions between
-- signal delivery and interrupt flag setting. The handler then chains to any
-- previously installed handler to preserve existing behavior.
--
-- This may be removed from the library interface in the future, as it is no
-- longer needed to be called explicitly.
installDefaultSigINTHandler :: IO ()
installDefaultSigINTHandler = do
  mainThread <- myThreadId
  weakId <- mkWeakThreadId mainThread
  let defaultHaskellHandler = do
        mt <- deRefWeak weakId
        for_ mt \t -> do
          throwTo t (toException UserInterrupt)

  -- Install synchronous C signal handlers first (these call triggerInterrupt immediately)
  for_ [sigINT, sigTERM, sigHUP] \sig -> do
    result <- [C.exp| int { hercules_install_signal_handler($(int sig)) } |]
    when (result /= 0) $
      panic $ "Failed to install synchronous signal handler for signal " <> show sig

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
