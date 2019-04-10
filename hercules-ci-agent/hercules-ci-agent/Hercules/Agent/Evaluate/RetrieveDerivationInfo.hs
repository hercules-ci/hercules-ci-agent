module Hercules.Agent.Evaluate.RetrieveDerivationInfo where

import           Protolude
import           Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
                                                ( DerivationInfo(DerivationInfo)
                                                )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
                                               as DerivationInfo
import qualified Nix.Derivation                as Drv
import           Data.Attoparsec.Text
import qualified Data.ByteString               as BS
import           Hercules.Error
import qualified Filesystem.Path.CurrentOS     as Deprecated.FilePath
                                                ( toText )
import qualified Data.Map                      as M
import           Hercules.Agent.Log
import           Control.Monad.Catch

retrieveDerivationInfo :: KatipContext m
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

  sourcePaths <- forM (toList $ Drv.inputSrcs drv) $ \path ->
    flip escalateAs (Deprecated.FilePath.toText path) $ \e ->
      FatalError
        $ toS
        $ "While getting source paths from "
        <> show drvPath
        <> ": "
        <> e

  inputDrvPaths <- forM (M.toList $ Drv.inputDrvs drv) $ \(path, outputs) ->
    fmap (, toList outputs)
      $ flip escalateAs (Deprecated.FilePath.toText path)
      $ \e ->
          FatalError
            $ toS
            $ "While getting source paths from "
            <> show path
            <> ": "
            <> e

  outputInfo <- forM (M.toList $ Drv.outputs drv) $ \(outputName, output) -> do
    path <-
      flip escalateAs (Deprecated.FilePath.toText (Drv.path output)) $ \e ->
        FatalError
          $ toS
          $ "While getting source paths from "
          <> show drvPath
          <> ": "
          <> e
    pure
      ( outputName
      , DerivationInfo.OutputInfo
        { DerivationInfo.path = path
        , DerivationInfo.isFixed = Drv.hashAlgo output /= ""
        }
      )

  pure $ DerivationInfo
    { derivationPath = drvPath
    , platform = Drv.platform drv
    , inputDerivations = M.fromList inputDrvPaths
    , inputSources = sourcePaths
    , outputs = M.fromList outputInfo
    }
