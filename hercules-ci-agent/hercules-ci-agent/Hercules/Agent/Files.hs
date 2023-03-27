{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Files where

import Control.Monad.IO.Unlift
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Env
import Protolude
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO.Temp

withWorkDir :: Text -> (FilePath -> App b) -> App b
withWorkDir hint f = do
  UnliftIO {unliftIO = unlift} <- askUnliftIO
  workDir <- asks (Config.workDirectory . config)
  liftIO $ withTempDirectory workDir (toS hint) $ unlift . f

readFileMaybe :: MonadIO m => FilePath -> m (Maybe Text)
readFileMaybe fp = liftIO do
  exists <- doesFileExist fp
  guard exists & traverse \_ -> readFile fp

renamePathTryHarder :: FilePath -> FilePath -> IO ()
renamePathTryHarder source dest = do
  catchJust
    ( \case
        e | isPermissionError e -> Just e
        _ -> Nothing
    )
    doIt
    (const tryHarder)
  where
    doIt = renamePath source dest
    modifyPermissions f p = getPermissions p >>= setPermissions p . f
    tryHarder = do
      modifyPermissions (setOwnerWritable True) (takeDirectory source)
      modifyPermissions (setOwnerWritable True) source
      modifyPermissions (setOwnerWritable True) (takeDirectory dest)
      doIt
