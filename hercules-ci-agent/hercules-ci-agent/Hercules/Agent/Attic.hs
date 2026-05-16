module Hercules.Agent.Attic
  ( push,
    substituterURL,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hercules.Agent.Env (App)
import Hercules.Agent.Log
import Hercules.CNix qualified as CNix
import Hercules.CNix.Store (StorePath)
import Hercules.Formats.AtticCache (AtticCache)
import Hercules.Formats.AtticCache qualified as AtticCache
import Protolude
import System.Directory (createDirectoryIfMissing)
import System.Environment qualified
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process

substituterURL :: AtticCache -> Text
substituterURL c =
  AtticCache.serverEndpoint c <> "/" <> AtticCache.cacheName c

-- Attic only supports loading the token and cache location from config file,
-- thus we need to create a synthetic one first.
-- TODO: Maybe attic can be improved for that?
push :: CNix.Store -> Text -> AtticCache -> [StorePath] -> App ()
push store localName cache paths = do
  pathStrings <-
    liftIO $
      mapM
        ( \p -> do
            bs <- CNix.storePathToPath store p
            return (decodeUtf8With lenientDecode bs)
        )
        paths
  logLocM DebugS (logStr ("Pushing to attic cache " <> localName))
  exitCode <- liftIO $ withSystemTempDirectory "hercules-attic" $ \cfgDir -> do
    let atticDir = cfgDir </> "attic"
    let cfgFile = atticDir </> "config.toml"
    createDirectoryIfMissing True atticDir
    T.writeFile cfgFile (renderConfig cache)
    setFileMode cfgFile 0o600
    oldEnv <- System.Environment.getEnvironment
    let isXdg k = k == "XDG_CONFIG_HOME" || k == "XDG_CACHE_HOME"
    let newEnv =
          [("XDG_CONFIG_HOME", cfgDir), ("XDG_CACHE_HOME", cfgDir)]
            ++ filter (\(k, _) -> not (isXdg k)) oldEnv
    let args =
          ["push", "--no-closure", toS (AtticCache.cacheName cache)]
            ++ map toS pathStrings
    let p = (proc "attic" args) {env = Just newEnv, close_fds = True}
    withCreateProcess p (\_ _ _ ph -> waitForProcess ph)
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure c -> throwIO $ FatalError $ "Attic push failed with exit code " <> show c

renderConfig :: AtticCache -> Text
renderConfig c =
  T.unlines
    [ "default-server = \"hercules\"",
      "",
      "[servers.hercules]",
      "endpoint = \"" <> tomlEscape (AtticCache.serverEndpoint c) <> "\"",
      "token = \"" <> tomlEscape (AtticCache.token c) <> "\""
    ]

tomlEscape :: Text -> Text
tomlEscape s =
  T.replace "\"" "\\\"" (T.replace "\\" "\\\\" s)
