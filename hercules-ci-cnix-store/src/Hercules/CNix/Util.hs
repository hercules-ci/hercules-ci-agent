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

{- ORMOLU_DISABLE -}
-- It doesn't like CPP

C.context context

C.include "<nix/util/signals.hh>"

C.include "signals.hxx"

C.using "namespace nix"

{-# DEPRECATED setInterruptThrown "Nix 2.33 removed `setInterruptThrown()`, and the Haskell function is a no-op when built against 2.33 or later versions. An OS thread can no longer opt out of Nix interrupt delivery. When Nix's process-global interrupt flag is set (e.g. with `triggerInterrupt`), `checkInterrupt()` throws at every call, except during C++ stack unwinding. No equivalent exists. Check whether you meant to use `triggerInterrupt` instead. If you are absolutely sure you need `setInterruptThrown` semantics for your bound thread, contribute to Nix." #-}
setInterruptThrown :: IO ()
setInterruptThrown =
#if NIX_IS_AT_LEAST(2, 33, 0)
  pass
#else
  [C.throwBlock| void {
    nix::setInterruptThrown();
  }|]
#endif

triggerInterrupt :: IO ()
triggerInterrupt =
  [C.throwBlock| void {
    nix::unix::triggerInterrupt();
  }|]

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
      panic $
        "Failed to install synchronous signal handler for signal " <> show sig

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
  }|]

#ifndef __GHCIDE__
foreign import ccall "wrapper"
  mkCallback :: IO () -> IO (FunPtr (IO ()))
#else
mkCallback :: IO () -> IO (FunPtr (IO ()))
mkCallback = panic "This is a stub to work around a ghcide issue. Please compile without -D__GHCIDE__"
#endif
