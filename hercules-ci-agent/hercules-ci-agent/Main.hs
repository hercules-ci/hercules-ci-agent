module Main
  ( main,
  )
where

import Hercules.Agent qualified
import Protolude
import System.Posix.Signals

main :: IO ()
main = do
  _ <- installHandler sigTERM (Catch $ raiseSignal sigINT) Nothing
  Hercules.Agent.main
