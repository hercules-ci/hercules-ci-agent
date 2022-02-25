-- |
--
--  Example:
--
--  @
--      main :: IO ()
--      main =
--        defaultMainWithHooks $
--          simpleUserHooks
--            & addHook
--              (mkSettings "nix-store")
--                { macroName = "NIX",
--                  flagPrefixName = "nix"
--                }
--  @
--
--  The above will look for a pkg-config package @nix-store@, and then
--
--    * Define CPP, C and C++ macros
--
--        * @NIX_MAJOR@, an integer
--        * @NIX_MINOR@, an integer
--        * @NIX_PATCH@, an integer; 0 if missing
--        * @NIX_IS_AT_LEAST(major,minor,patch)@, returning true when the discovered version @>=@ the specified version
--
--    * Set or unset flags like `nix-2_4` so that the flag is true when the
--      discovered version is at least the version in the flag's name.
module Distribution.PkgConfigVersionHook
  ( addHook,
    mkSettings,
    Settings (..),
    composeConfHook,
  )
where

import Control.Lens ((%~), (^.))
import Control.Monad (when)
import qualified Data.Char as C
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.List as L
import Distribution.Simple
import Distribution.Simple.Setup (configConfigurationsFlags)
import Distribution.Types.BuildInfo.Lens (cppOptions, cxxOptions)
import Distribution.Types.Flag (flagName, mkFlagAssignment, mkFlagName, unFlagName)
import Distribution.Types.GenericPackageDescription.Lens
  ( allCondTrees,
    condBenchmarks,
    condExecutables,
    condForeignLibs,
    condLibrary,
    condSubLibraries,
    condTestSuites,
    genPackageFlags,
  )
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import qualified Text.ParserCombinators.ReadP as P
import Prelude hiding (log)

addHook :: Settings -> UserHooks -> UserHooks
addHook settings hooks = hooks {confHook = composeConfHook settings (confHook hooks)}

data Settings = Settings
  { pkgConfigName :: String,
    macroName :: String,
    flagPrefixName :: String
  }

mkSettings :: String -> Settings
mkSettings name =
  Settings
    { pkgConfigName = name,
      macroName = map (\x -> case x of '-' -> '_') name,
      flagPrefixName = name
    }

composeConfHook settings origHook = \(genericPackageDescription, hookedBuildInfo) confFlags -> do
  (actualMajor, actualMinor, actualPatch) <- getPkgConfigPackageVersion (pkgConfigName settings)

  let defines =
        [ "-D" <> macroName settings <> "_MAJOR=" <> show actualMajor,
          "-D" <> macroName settings <> "_MINOR=" <> show actualMinor,
          "-D" <> macroName settings <> "_PATCH=" <> show actualPatch,
          "-D" <> macroName settings <> "_IS_AT_LEAST(a,b,c)=(" <> show actualMajor <> ">a||(" <> show actualMajor <> "==a&&(" <> show actualMinor <> ">b||(" <> show actualMinor <> "==b&&" <> show actualPatch <> ">=c))))"
        ]
      extraFlags =
        [ (mkFlagName (flagPrefixName settings ++ "-" ++ show major ++ "_" ++ show minor), (actualMajor, actualMinor) >= (major, minor))
          | declaredFlag <- genericPackageDescription ^. genPackageFlags,
            let rawName = unFlagName $ flagName declaredFlag,
            rawVersion <- L.stripPrefix (flagPrefixName settings ++ "-") rawName & toList,
            [major, minor] <- unambiguously parseFlagVersion rawVersion & toList
        ]
      setDefines comp x =
        x
          & comp . cppOptions %~ (<> defines)
          & comp . cxxOptions %~ (<> defines)
      genericPackageDescription' =
        genericPackageDescription
          & setDefines (condLibrary . traverse . traverse)
          & setDefines (condSubLibraries . traverse . traverse . traverse)
          & setDefines (condForeignLibs . traverse . traverse . traverse)
          & setDefines (condExecutables . traverse . traverse . traverse)
          & setDefines (condTestSuites . traverse . traverse . traverse)
          & setDefines (condBenchmarks . traverse . traverse . traverse)

      configConfigurationsFlags' = configConfigurationsFlags confFlags `mappend` mkFlagAssignment extraFlags
      confFlags' =
        confFlags
          { configConfigurationsFlags = configConfigurationsFlags'
          }
  origHook (genericPackageDescription', hookedBuildInfo) confFlags'

parseVersion :: P.ReadP [Int]
parseVersion = do
  map read <$> do
    P.many1 (P.satisfy C.isDigit) `P.sepBy` P.char '.'

parseFlagVersion :: P.ReadP [Int]
parseFlagVersion =
  map read <$> do
    P.many1 (P.satisfy C.isDigit) `P.sepBy` P.char '_'

unambiguously :: P.ReadP a -> String -> Maybe a
unambiguously p s =
  case filter (\(a, x) -> x == "") $ P.readP_to_S p s of
    [(v, _)] -> Just v
    _ -> Nothing

getPkgConfigPackageVersion :: String -> IO (Int, Int, Int)
getPkgConfigPackageVersion pkgName = do
  s <- readProcess "pkg-config" ["--modversion", pkgName] ""
  case L.sortOn (\(_, s) -> length s) $ P.readP_to_S parseVersion s of
    [] -> error ("Could not parse version " ++ show s ++ " returned by pkg-config for package " ++ pkgName)
    (v, r) : _ -> do
      when (L.dropWhile C.isSpace r /= "") $ do
        log ("ignoring trailing text " ++ show r ++ " in version " ++ show s ++ " of pkg-config package " ++ pkgName)
      let v' = v ++ L.repeat 0
      pure (v' L.!! 0, v' L.!! 1, v' L.!! 2)

-- Should probably use a Cabal function?
log = hPutStrLn stderr
