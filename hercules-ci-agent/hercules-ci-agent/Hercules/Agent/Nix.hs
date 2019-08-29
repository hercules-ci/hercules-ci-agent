module Hercules.Agent.Nix where

import Hercules.Agent.Env as Agent.Env
import Hercules.Agent.Nix.Env as Nix.Env
import Protolude
import System.Process (readProcess)
import qualified System.Process

withExtraOptions :: [(Text, Text)] -> App a -> App a
withExtraOptions extraOpts = local $ \env ->
  env
    { nixEnv =
        (nixEnv env)
          { extraOptions = extraOptions (nixEnv env) <> extraOpts
            }
      }

askExtraOptions :: MonadReader Agent.Env.Env m => m [(Text, Text)]
askExtraOptions = asks (extraOptions . nixEnv)

getExtraOptionArguments :: MonadReader Agent.Env.Env m => m [Text]
getExtraOptionArguments = do
  asks (f . extraOptions . nixEnv)
  where
    f = concatMap $ \(opt, val) -> ["--option", opt, val]

readNixProcess
  :: Text -- ^ Command
  -> [Text] -- ^ Options
  -> [Text] -- ^ Paths
  -> Text -- ^ Stdin
  -> App Text
readNixProcess cmd opts paths stdinText = do
  extraOpts <- getExtraOptionArguments
  toSL <$> liftIO (readProcess (toSL cmd) (map toSL (opts <> extraOpts <> ["--"] <> paths)) (toSL stdinText))

nixProc :: Text -> [Text] -> [Text] -> App System.Process.CreateProcess
nixProc exe opts args = do
  extraOpts <- getExtraOptionArguments
  pure $ System.Process.proc (toSL exe) (map toSL (opts <> extraOpts <> ["--"] <> args))
