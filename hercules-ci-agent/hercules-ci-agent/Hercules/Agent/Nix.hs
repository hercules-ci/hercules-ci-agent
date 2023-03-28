module Hercules.Agent.Nix where

import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Hercules.Agent.Env as Agent.Env
import Hercules.Agent.EnvironmentInfo (getNixInfo, nixNetrcFile)
import Hercules.Agent.Nix.Env as Nix.Env
import Protolude
import System.Directory (doesFileExist)

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

getNetrcLines :: App [Text]
getNetrcLines = liftIO $ do
  info <- getNixInfo
  let fm = toS . decodeUtf8With lenientDecode <$> nixNetrcFile info
  fmap fold <$> runMaybeT $ do
    f <- MaybeT $ pure fm
    exs <- lift $ doesFileExist f
    guard exs
    bs <- liftIO $ BS.readFile f
    pure $
      bs
        & decodeUtf8
        & T.lines
