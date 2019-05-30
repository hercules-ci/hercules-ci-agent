module Hercules.Agent.SecureDirectory where

import Protolude

import           System.FilePath ((</>))
import qualified System.Directory
import qualified System.IO.Temp
import qualified System.Posix.Files as Posix
import           Control.Monad.Catch (MonadMask)

init :: IO ()
init = do
  d <- getSecureDirectory
  System.Directory.createDirectoryIfMissing True d
  -- ideally this would be atomic
  Posix.setFileMode d Posix.ownerModes -- owner only

getSecureDirectory :: IO FilePath
getSecureDirectory =
  System.Directory.getAppUserDataDirectory "hercules-ci-agent" <&> (</> "secure")

withSecureTempFile
  :: (MonadIO m, MonadMask m)
  => [Char] -- ^ file name template
  -> (FilePath -> Handle -> m a)
  -> m a
withSecureTempFile tpl m = do
  d <- liftIO getSecureDirectory
  System.IO.Temp.withTempFile d tpl m
