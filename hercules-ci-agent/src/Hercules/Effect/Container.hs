{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Effect.Container where

import Control.Lens
import Data.Aeson (Value (String), eitherDecode, encode, object, toJSON)
import Data.Aeson.Lens
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import GHC.IO.Exception (IOErrorType (HardwareFault))
import Protolude
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (ioeGetErrorType)
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process (CreateProcess (..), StdStream (UseHandle), proc, waitForProcess, withCreateProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)

data BindMount = BindMount
  { pathInContainer :: Text,
    pathInHost :: Text,
    readOnly :: Bool
  }

defaultBindMount :: Text -> BindMount
defaultBindMount path = BindMount {pathInContainer = path, pathInHost = path, readOnly = True}

data Config = Config
  { extraBindMounts :: [BindMount],
    executable :: Text,
    arguments :: [Text],
    environment :: Map Text Text,
    workingDirectory :: Text,
    hostname :: Text,
    rootReadOnly :: Bool,
    virtualUID :: Int,
    virtualGID :: Int,
    hostUID :: Int,
    hostGID :: Int
  }

effectToOCIRuntimeSpec :: Config -> Value -> Value
effectToOCIRuntimeSpec config spec =
  let defaultMounts = [defaultBindMount "/nix/store"]
      mounts =
        foldMap
          ( \bindMount ->
              pure $
                object
                  [ ("destination", String $ pathInContainer bindMount),
                    ("source", String $ pathInHost bindMount),
                    ("type", "bind"),
                    ( "options",
                      toJSON $
                        ["bind" :: Text]
                          <> ["ro" | readOnly bindMount]
                    )
                  ]
          )
          (defaultMounts <> extraBindMounts config)
   in spec
        & key "process" . key "args" .~ toJSON ([executable config] <> arguments config)
        & key "mounts" . _Array %~ (<> mounts)
        & key "process" . key "terminal" .~ toJSON True
        & key "process" . key "env" .~ toJSON (config & environment & M.toList <&> \(k, v) -> k <> "=" <> v)
        & key "process" . key "cwd" .~ toJSON (config & workingDirectory)
        & key "process" . key "user" . key "uid" .~ toJSON (virtualUID config)
        & key "process" . key "user" . key "gid" .~ toJSON (virtualGID config)
        & key "process" . key "user" . key "umask" .~ toJSON (0o077 :: Int)
        & key "process" . key "user" . key "additionalGids" . _Array .~ V.fromList []
        & key "linux" . _Object . at "uidMappings"
          ?~ toJSON
            [ object
                [ ("containerID", toJSON (virtualUID config)),
                  ("hostID", toJSON (hostUID config)),
                  ("size", toJSON (1 :: Int))
                ]
            ]
        & key "linux" . _Object . at "gidMappings"
          ?~ toJSON
            [ object
                [ ("containerID", toJSON (virtualGID config)),
                  ("hostID", toJSON (hostGID config)),
                  ("size", toJSON (1 :: Int))
                ]
            ]
        & key "hostname" .~ toJSON (config & hostname)
        & key "root" . key "readonly" .~ toJSON (config & rootReadOnly)
        -- TODO Use slirp? e.g. https://github.com/rootless-containers/slirp4netns or might kernel offer bridging (in the future?)
        & key "linux" . key "namespaces" . _Array %~ V.filter (\x -> x ^? key "type" . _String /= Just "network")

run :: FilePath -> Config -> IO ExitCode
run dir config = do
  let containerRuntimeExe = "crun"
      createConfigJsonSpec =
        (System.Process.proc containerRuntimeExe ["spec", "--rootless"])
          { cwd = Just dir
          }
      configJsonPath = dir </> "config.json"
      runtimeRootPath = dir </> "container-root"
      rootfsPath = dir </> "rootfs"
  (exit, _out, err) <- readCreateProcessWithExitCode createConfigJsonSpec ""
  case exit of
    ExitSuccess -> pass
    ExitFailure e -> do
      putErrText (decodeUtf8With lenientDecode err)
      panic $ "Could not create container configuration template. " <> toS containerRuntimeExe <> " terminated with exit code " <> show e
  templateBytes <- BS.readFile configJsonPath
  template <- case eitherDecode (BL.fromStrict templateBytes) of
    Right a -> pure a
    Left e -> throwIO (FatalError $ "decoding container config.json template: " <> show e)
  let configJson = effectToOCIRuntimeSpec config template
  BS.writeFile configJsonPath (BL.toStrict $ encode configJson)
  createDirectory rootfsPath
  createDirectory runtimeRootPath
  name <- do
    uuid <- UUID.nextRandom
    pure $ "hercules-ci-" <> show uuid
  (exitCode, _) <- withPseudoTerminalHandles $
    \(master, terminal) -> do
      concurrently
        ( do
            let createProcSpec =
                  (System.Process.proc containerRuntimeExe ["--root", runtimeRootPath, "run", name])
                    { std_in = UseHandle terminal, -- can't pass /dev/null :(
                      std_out = UseHandle terminal,
                      std_err = UseHandle terminal,
                      cwd = Just dir
                    }
            withCreateProcess createProcSpec \_subStdin _noOut _noErr processHandle -> do
              waitForProcess processHandle
                `onException` ( do
                                  putErrText "Terminating effect process..."
                                  _ <- System.Process.withCreateProcess (System.Process.proc containerRuntimeExe ["kill", name]) \_ _ _ kh ->
                                    waitForProcess kh
                                  threadDelay 3_000_000
                                  _ <- System.Process.withCreateProcess (System.Process.proc containerRuntimeExe ["kill", name, "KILL"]) \_ _ _ kh ->
                                    waitForProcess kh
                                  putErrText "Killed effect process."
                              )
        )
        ( do
            let shovel =
                  handleEOF (BS.hGetLine master) >>= \case
                    "" -> pass
                    someBytes | "@nix" `BS.isPrefixOf` someBytes -> do
                      -- TODO use it (example @nix { "action": "setPhase", "phase": "effectPhase" })
                      shovel
                    someBytes -> do
                      BS.hPut stderr (someBytes <> "\n")
                      shovel
                handleEOF = handle \e -> if ioeGetErrorType e == HardwareFault then pure "" else throwIO e
            shovel
        )
  pure exitCode

-- | Like 'openPseudoTerminalHandles' but closes the handles after the
-- function is done.
withPseudoTerminalHandles :: ((Handle, Handle) -> IO a) -> IO a
withPseudoTerminalHandles =
  bracket
    openPseudoTerminalHandles
    ( \(master, terminal) -> do
        hClose master `catch` \(_ :: SomeException) -> pass
        hClose terminal `catch` \(_ :: SomeException) -> pass
    )

-- | Like 'openPseudoTerminal' but returning handles, in a resource-safe manner.
openPseudoTerminalHandles :: IO (Handle, Handle)
openPseudoTerminalHandles =
  mask_ do
    (masterFd, terminalFd) <- openPseudoTerminal

    ( do
        master <- fdToHandle masterFd
        terminal <- fdToHandle terminalFd
        pure (master, terminal)
      )
      `onException` do
        closeFd masterFd
        when (terminalFd /= masterFd) (closeFd terminalFd)
