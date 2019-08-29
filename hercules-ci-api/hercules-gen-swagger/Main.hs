module Main
  ( main
    )
where

import Data.Aeson (encode)
import Data.String.Conv (toS)
import Hercules.API (swagger)
import Prelude

main :: IO ()
main = putStrLn $ toS $ encode swagger
