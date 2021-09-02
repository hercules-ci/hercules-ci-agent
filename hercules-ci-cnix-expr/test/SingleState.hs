-- | Work around Nix regression causing corruption when EvalState is destroyed and a new one created.
module SingleState (withGlobalState, evalState) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hercules.CNix.Expr
import Hercules.CNix.Store.TestUtil (withTempStore)
import Protolude hiding (evalState, state)
import System.IO.Unsafe (unsafePerformIO)

theEvalState :: IORef (Ptr EvalState)
theEvalState = unsafePerformIO $ newIORef (panic "Store not initialized yet")
{-# NOINLINE theEvalState #-}

-- | Work around Nix regression causing corruption when EvalState is destroyed and a new one created.
withGlobalState :: IO () -> IO ()
withGlobalState io = withTempStore $ \store -> withEvalState store $ \state -> do
  writeIORef theEvalState state
  io

-- | Work around Nix regression causing corruption when EvalState is destroyed and a new one created.
evalState :: Ptr EvalState
evalState = unsafePerformIO $ readIORef theEvalState
{-# NOINLINE evalState #-}
