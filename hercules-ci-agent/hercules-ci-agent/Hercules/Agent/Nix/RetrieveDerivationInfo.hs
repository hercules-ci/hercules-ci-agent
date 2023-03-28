{-# LANGUAGE BlockArguments #-}

module Hercules.Agent.Nix.RetrieveDerivationInfo where

import Data.Map qualified as M
import Data.Text qualified as T
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
  ( DerivationInfo (DerivationInfo),
  )
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo qualified as DerivationInfo
import Hercules.CNix
import Protolude

retrieveDerivationInfo ::
  MonadIO m =>
  Store ->
  StorePath ->
  m DerivationInfo
retrieveDerivationInfo store drvPath = liftIO $ do
  drv <- getDerivation store drvPath
  path <- storePathToPath store drvPath
  drvName <- getDerivationNameFromPath drvPath
  retrieveDerivationInfo' store path drvName drv

retrieveDerivationInfo' :: Store -> ByteString -> ByteString -> Derivation -> IO DerivationInfo
retrieveDerivationInfo' store drvPath drvName drv = do
  sourceStorePaths <- getDerivationSources store drv
  inputDrvStorePaths <- getDerivationInputs store drv
  outputs <- getDerivationOutputs store drvName drv
  env <- getDerivationEnv drv
  platform <- getDerivationPlatform drv
  let requiredSystemFeatures = maybe [] splitFeatures $ M.lookup "requiredSystemFeatures" env
      splitFeatures = filter (not . T.null) . T.split (== ' ') . decode
      decode = decodeUtf8With lenientDecode
  inputDrvPaths <- for inputDrvStorePaths \(storePath, inputOutputs) -> do
    path <- Hercules.CNix.storePathToPath store storePath
    pure (path, inputOutputs)
  sourcePaths <- for sourceStorePaths (Hercules.CNix.storePathToPath store)
  outputs' <- for outputs \output -> do
    path <- for (derivationOutputPath output) (Hercules.CNix.storePathToPath store)
    let isFixed = case derivationOutputDetail output of
          (DerivationOutputInputAddressed _s) -> False
          (DerivationOutputCAFixed _f _s) -> True
          (DerivationOutputCAFloating _f _h) -> False
          DerivationOutputDeferred -> False
    pure
      ( decode $ derivationOutputName output,
        DerivationInfo.OutputInfo
          { DerivationInfo.path = decode <$> path,
            DerivationInfo.isFixed = isFixed
          }
      )
  pure $
    DerivationInfo
      { derivationPath = decode drvPath,
        platform = decode platform,
        requiredSystemFeatures = requiredSystemFeatures,
        inputDerivations = inputDrvPaths & map (bimap decode (map decode)) & M.fromList,
        inputSources = map decode sourcePaths,
        outputs =
          outputs' & M.fromList
      }
