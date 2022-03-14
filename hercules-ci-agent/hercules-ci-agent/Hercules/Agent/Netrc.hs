{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Netrc where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Netrc.Env (Env (Env, netrcFile))
import qualified Hercules.Agent.Nix as Nix
import qualified Hercules.Agent.SecureDirectory as SecureDirectory
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
