{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Netrc where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.Netrc.Env (Env (Env, netrcFile))
import Hercules.Agent.Nix qualified as Nix
import Hercules.Agent.SecureDirectory qualified as SecureDirectory
import Protolude
import System.IO (hClose)

withNixNetrc :: Env.App a -> Env.App a
withNixNetrc m = do
  baseLns <- Nix.getNetrcLines
  SecureDirectory.withSecureTempFile "tmp-netrc.key" $ \netrcPath netrcHandle -> do
    liftIO $ do
      T.hPutStrLn netrcHandle (T.unlines baseLns)
      hClose netrcHandle
    local (\env -> env {Env.netrcEnv = Env {netrcFile = Just netrcPath}}) m

getNetrcFile :: Env.App FilePath
getNetrcFile = do
  env <- asks Env.netrcEnv
  case netrcFile env of
    Nothing -> throwIO $ FatalError "getNetrcFile is only valid in withNixNetrc"
    Just f -> pure f

appendLines :: [Text] -> Env.App ()
appendLines lns = do
  f <- getNetrcFile
  liftIO do
    withFile f AppendMode \h -> do
      T.hPutStrLn h ("\n" <> T.unlines lns)
