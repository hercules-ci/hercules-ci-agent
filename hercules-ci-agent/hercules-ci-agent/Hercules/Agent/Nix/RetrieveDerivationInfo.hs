module Hercules.Agent.Nix.RetrieveDerivationInfo where

import Control.Monad.Catch
import Data.Attoparsec.Text
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as Deprecated.FilePath
  ( toText
    )
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
  ( DerivationInfo (DerivationInfo)
    )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo as DerivationInfo
import Hercules.Agent.Log
import Hercules.Error
import qualified Nix.Derivation as Drv
import Protolude

retrieveDerivationInfo
  :: KatipContext m
  => MonadIO m
  => MonadThrow m
  => DerivationInfo.DerivationPathText
  -> m DerivationInfo
retrieveDerivationInfo drvPath = do
  drvBytes <- liftIO $ BS.readFile (toSL drvPath)
  let drvText = toSL drvBytes
      res = Data.Attoparsec.Text.parse Drv.parseDerivation drvText
  drv <-
    escalateAs
      (\e -> FatalError $ toS $ "While parsing " <> show drvPath <> ": " <> e)
      $ eitherResult res
  sourcePaths <-
    forM (toList $ Drv.inputSrcs drv) $ \path ->
      flip escalateAs (Deprecated.FilePath.toText path) $ \e ->
        FatalError
          $ toS
          $ "While getting source paths from "
          <> show drvPath
          <> ": "
          <> e
  inputDrvPaths <-
    forM (M.toList $ Drv.inputDrvs drv) $ \(path, outputs) ->
      fmap (,toList outputs)
        $ flip escalateAs (Deprecated.FilePath.toText path)
        $ \e ->
          FatalError
            $ toS
            $ "While getting source paths from "
            <> show path
            <> ": "
            <> e
  outputInfo <-
    forM (M.toList $ Drv.outputs drv) $ \(outputName, output) -> do
      path <-
        flip escalateAs (Deprecated.FilePath.toText (Drv.path output)) $ \e ->
          FatalError
            $ toS
            $ "While getting source paths from "
            <> show drvPath
            <> ": "
            <> e
      pure
        ( outputName,
          DerivationInfo.OutputInfo
            { DerivationInfo.path = path,
              DerivationInfo.isFixed = Drv.hashAlgo output /= ""
              }
          )
  let requiredSystemFeatures = maybe [] splitFeatures $ M.lookup "requiredSystemFeatures" (Drv.env drv)
      splitFeatures = filter (not . T.null) . T.split (== ' ')
  pure $ DerivationInfo
    { derivationPath = drvPath,
      platform = Drv.platform drv,
      requiredSystemFeatures = requiredSystemFeatures,
      inputDerivations = M.fromList inputDrvPaths,
      inputSources = sourcePaths,
      outputs = M.fromList outputInfo
      }
