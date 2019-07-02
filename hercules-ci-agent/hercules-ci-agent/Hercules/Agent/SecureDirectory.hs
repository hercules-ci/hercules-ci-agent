module Hercules.Agent.SecureDirectory where

import Protolude

import           Hercules.Agent.Config (FinalConfig, workDirectory)
import           Hercules.Agent.Env
import           System.FilePath ((</>))
import qualified System.Directory
import qualified System.IO.Temp
import qualified System.Posix.Files as Posix

init :: FinalConfig -> IO ()
init cfg = do
  d <- getSecureDirectory cfg
  System.Directory.createDirectoryIfMissing True d
  -- ideally this would be atomic
  Posix.setFileMode d Posix.ownerModes -- owner only

getSecureDirectory :: FinalConfig -> IO FilePath
getSecureDirectory cfg = pure $ workDirectory cfg </> "secure"

withSecureTempFile
  :: [Char] -- ^ file name template
  -> (FilePath -> Handle -> App a)
  -> App a
withSecureTempFile tpl m = do
  cfg <- asks config
  d <- liftIO (getSecureDirectory cfg)
  System.IO.Temp.withTempFile d tpl m
