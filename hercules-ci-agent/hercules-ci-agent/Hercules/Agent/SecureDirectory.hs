module Hercules.Agent.SecureDirectory where

import Hercules.Agent.Config (FinalConfig, workDirectory)
import Hercules.Agent.Env
import Protolude
import System.Directory qualified
import System.FilePath ((</>))
import System.IO.Temp qualified
import System.Posix.Files qualified as Posix

init :: FinalConfig -> IO ()
init cfg = do
  d <- getSecureDirectory cfg
  System.Directory.createDirectoryIfMissing True d
  -- ideally this would be atomic
  Posix.setFileMode d Posix.ownerModes -- owner only

getSecureDirectory :: FinalConfig -> IO FilePath
getSecureDirectory cfg = pure $ workDirectory cfg </> "secure"

withSecureTempFile ::
  -- | file name template
  [Char] ->
  (FilePath -> Handle -> App a) ->
  App a
withSecureTempFile tpl m = do
  cfg <- asks config
  d <- liftIO (getSecureDirectory cfg)
  System.IO.Temp.withTempFile d tpl m
