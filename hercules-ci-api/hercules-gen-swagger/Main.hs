module Main
  ( main,
  )
where

import Data.Aeson (encode)
import Data.String.Conv (toS)
import Hercules.API (openapi3, swagger)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--openapi3"] -> putStrLn $ toS $ encode openapi3
    [] -> putStrLn $ toS $ encode swagger
    _ -> error $ "Unknown arguments" <> show args
