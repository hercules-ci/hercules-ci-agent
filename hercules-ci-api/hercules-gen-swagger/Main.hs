module Main
  ( main
  )
where

import           Prelude
import           Data.Aeson                     ( encode )
import           Hercules.API                   ( swagger )


main :: IO ()
main = print $ encode swagger
