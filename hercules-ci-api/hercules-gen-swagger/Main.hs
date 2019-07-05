module Main
  ( main
  )
where

import           Prelude
import           Data.Aeson                     ( encode )
import           Hercules.API                   ( swagger )
import           Data.String.Conv               ( toS )

main :: IO ()
main = putStrLn $ toS $ encode swagger
