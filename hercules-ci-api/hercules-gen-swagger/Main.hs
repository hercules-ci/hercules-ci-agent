module Main
  ( main,
  )
where

import Data.Aeson (encode)
import Data.Function ((&))
import Data.String.Conv (toS)
import Data.Text qualified as T
import Hercules.API (openapi3, swagger)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--experimental-openapi3"] ->
      putStrLn $
        openapi3
          & encode
          & toS
          & T.replace "Attribute_(Result_AttributeError_Derivation)" "Attribute_Result_AttributeError_Derivation"
          & toS
    [] -> putStrLn $ toS $ encode swagger
    _ -> do
      hPutStrLn stderr "Usage: hercules-gen-swagger [--experimental-openapi3] > swagger.json"
      error $ "Unknown arguments" <> show args
