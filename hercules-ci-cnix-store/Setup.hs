import Data.Function ((&))
import Distribution.PkgConfigVersionHook as PV
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory (createDirectoryIfMissing)
import System.Process (callProcess)

main :: IO ()
main =
  defaultMainWithHooks $
    simpleUserHooks
      { preBuild = \args flags -> do
          generateBindings (fromFlag $ buildVerbosity flags)
          preBuild simpleUserHooks args flags
      }
      & PV.addHook
        (PV.mkSettings "nix-store")
          { PV.macroName = "NIX",
            PV.flagPrefixName = "nix"
          }

generateBindings :: Verbosity -> IO ()
generateBindings verbosity = do
  notice verbosity "Generating Nix C API bindings with hs-bindgen..."
  createDirectoryIfMissing True "src-generated"
  callProcess
    "hs-bindgen-cli"
    [ "preprocess",
      "-I",
      "/nix/store/3bgmbyiinaq8z420gjg8bz2pqf0dpzyp-nix-2.28.5-dev/include",
      "--hs-output-dir",
      "src-generated",
      "--create-output-dirs",
      "--module",
      "Hercules.CNix.Nix.API.Store",
      "--unique-id",
      "com.hercules-ci.cnix-store",
      "--enable-program-slicing",
      "/nix/store/3bgmbyiinaq8z420gjg8bz2pqf0dpzyp-nix-2.28.5-dev/include/nix_api_store.h"
    ]
