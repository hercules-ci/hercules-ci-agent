module Main
  ( main
  )
where

import           Protolude
import qualified Hercules.Agent
import           System.Posix.Signals


main :: IO ()
main = do
  installHandler sigTERM (Catch $ raiseSignal sigINT) Nothing
  Hercules.Agent.main
