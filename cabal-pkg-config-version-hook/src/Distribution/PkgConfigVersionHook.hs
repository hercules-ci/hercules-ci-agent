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
import Data.Char (isSpace)
import qualified Data.Char as C
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.List as L
import Distribution.Simple
import Distribution.Simple.Configure (configCompilerAuxEx)
import Distribution.Simple.Program (ConfiguredProgram, getProgramOutput, needProgram, pkgConfigProgram)
import Distribution.Simple.Setup (configConfigurationsFlags, configPrograms, configVerbosity, fromFlag)
import Distribution.Types.BuildInfo.Lens (ccOptions, cppOptions, cxxOptions)
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
import Distribution.Verbosity (Verbosity)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import qualified Text.ParserCombinators.ReadP as P
import Prelude hiding (log)

-- | Hook into Cabal to provide pkg-config metadata. Can be applied multiple
-- times to support multiple packages.
addHook :: Settings -> UserHooks -> UserHooks
addHook settings hooks = hooks {confHook = composeConfHook settings (confHook hooks)}

-- | How the metadata for a pkg-config package should be made available to the
-- cabal file.
data Settings = Settings
  { -- | Name of the package; used for querying pkg-config.
    pkgConfigName :: String,
    -- | Name to use in the Haskell CPP and C/C++ preprocessor macros.
    --
    -- For example, `pkgConfigName = "FOO"` will set the macros
    --
    --  * @FOO_MAJOR@
    --
    --  * @FOO_MINOR@
    --
    --  * @FOO_PATCH@
    --
    --  * @FOO_IS_AT_LEAST(major, minor, patch)@
    macroName :: String,
    -- | Name to use when setting flag values in the cabal file.
    --
    -- Flags named with this prefix, followed by a dash, followed by a major version number, an underscore and a minor version number will be set when the detected package is at least that version.
    flagPrefixName :: String
  }

-- | Derive a default 'Settings' value from just a pkg-config package name.
mkSettings :: String -> Settings
mkSettings name =
  Settings
    { pkgConfigName = name,
      macroName = map (\x -> case x of '-' -> '_') name,
      flagPrefixName = name
    }

-- | Extend the value of 'confHook'. It's what powers 'addHook'.
composeConfHook settings origHook = \(genericPackageDescription, hookedBuildInfo) confFlags -> do
  (_compiler, _platform, programDb) <- configCompilerAuxEx confFlags
  let verbosity = fromFlag (configVerbosity confFlags)
  mpkgConfig <- needProgram verbosity pkgConfigProgram programDb
  pkgConfig <- case mpkgConfig of
    Nothing -> error "Cannot find pkg-config program."
    Just (pkgConfig, _) -> pure pkgConfig

  (actualMajor, actualMinor, actualPatch) <- getPkgConfigPackageVersion verbosity pkgConfig (pkgConfigName settings)

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
          & comp . ccOptions %~ (<> defines)
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

getPkgConfigPackageVersion :: Verbosity -> ConfiguredProgram -> String -> IO (Int, Int, Int)
getPkgConfigPackageVersion verbosity pkgConfig pkgName = do
  s <- trim <$> getProgramOutput verbosity pkgConfig ["--modversion", pkgName]
  case L.sortOn (\(_, s) -> length s) $ P.readP_to_S parseVersion s of
    [] -> error ("Could not parse version " ++ show s ++ " returned by pkg-config for package " ++ pkgName)
    (v, r) : _ -> do
      when (L.dropWhile isSpace r /= "") $ do
        log ("ignoring trailing text " ++ show r ++ " in version " ++ show s ++ " of pkg-config package " ++ pkgName)
      let v' = v ++ L.repeat 0
      pure (v' L.!! 0, v' L.!! 1, v' L.!! 2)
  where
    trim = L.dropWhile isSpace . L.dropWhileEnd isSpace

-- Should probably use a Cabal function?
log = hPutStrLn stderr
