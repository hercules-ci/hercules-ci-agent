import Data.Function ((&))
import Distribution.PkgConfigVersionHook as PV
import Distribution.Simple

main :: IO ()
main =
  defaultMainWithHooks $
    simpleUserHooks
      & PV.addHook
        (PV.mkSettings "nix-store")
          { PV.macroName = "NIX",
            PV.flagPrefixName = "nix"
          }
