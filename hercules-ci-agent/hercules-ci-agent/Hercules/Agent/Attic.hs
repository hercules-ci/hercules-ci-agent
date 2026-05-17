module Hercules.Agent.Attic
  ( push,
    substituterURL,
    toNetrcLines,
  )
where

import Data.Map (singleton)
import Data.Map qualified as M
import Data.Text.IO qualified as T
import Hercules.Agent.Env (App)
import Hercules.Agent.Log
import Hercules.CNix qualified as CNix
import Hercules.CNix.Store (StorePath)
import Hercules.Formats.AtticCache (AtticCache)
import Hercules.Formats.AtticCache qualified as AtticCache
import Network.URI (URIAuth (uriPort, uriRegName), parseURI, uriAuthority)
import Protolude
import System.Directory (createDirectoryIfMissing)
import System.Environment qualified
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process hiding (readCreateProcessWithExitCode)
import System.Process.ByteString (readCreateProcessWithExitCode)
import Toml (TomlCodec, (.=))
import Toml qualified

substituterURL :: AtticCache -> Text
substituterURL c =
  AtticCache.serverEndpoint c <> "/" <> AtticCache.cacheName c

-- TODO: Should reject attic caches with the same endpoint, given that
-- netrc is hostname-based, but attic caches are path-based.
-- The token leak should not cause any problems here,
-- but user might encounter authorization failure. Does netrc even works with the
-- duplicate machine entry?
toNetrcLines :: Map Text AtticCache -> [Text]
toNetrcLines = mapMaybe toLine . M.elems
  where
    toLine cache = do
      uri <- parseURI (toS (AtticCache.serverEndpoint cache))
      auth <- uriAuthority uri
      let host = toS (uriRegName auth <> uriPort auth) :: Text
      pure $ "machine " <> host <> " password " <> AtticCache.token cache

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
  (exitCode, _out, err) <- liftIO $ withSystemTempDirectory "hercules-attic" $ \cfgDir -> do
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
    readCreateProcessWithExitCode p ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure c -> throwIO $ FatalError $ "Attic push failed with exit code " <> show c <> ", stderr: " <> (decodeUtf8With lenientDecode err)

data AtticServer = AtticServer
  { endpoint :: Text,
    token :: Text
  }

data AtticClientConfig = AtticClientConfig
  { defaultServer :: Text,
    servers :: Map Text AtticServer
  }

atticServerCodec :: TomlCodec AtticServer
atticServerCodec =
  AtticServer
    <$> Toml.text "endpoint"
    .= endpoint
    <*> Toml.text "token" .= token

atticClientConfigCodec :: TomlCodec AtticClientConfig
atticClientConfigCodec =
  AtticClientConfig
    <$> Toml.text "default-server"
    .= defaultServer
    <*> Toml.tableMap Toml._KeyText (Toml.table atticServerCodec) "servers" .= servers

renderConfig :: AtticCache -> Text
renderConfig c =
  Toml.encode atticClientConfigCodec $
    AtticClientConfig
      { defaultServer = "hercules",
        servers =
          singleton "hercules" $
            AtticServer
              { endpoint = AtticCache.serverEndpoint c,
                token = AtticCache.token c
              }
      }
