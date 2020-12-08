module Hercules.Agent.Nix.RetrieveDerivationInfo where

import CNix
import CNix.Internal.Context (Derivation)
import qualified Data.Map as M
import qualified Data.Text as T
import Foreign.ForeignPtr (ForeignPtr)
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
  ( DerivationInfo (DerivationInfo),
  )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo as DerivationInfo
import Protolude

retrieveDerivationInfo ::
  MonadIO m =>
  Ptr (Ref NixStore) ->
  DerivationInfo.DerivationPathText ->
  m DerivationInfo
retrieveDerivationInfo store drvPath = liftIO $ do
  drv <- getDerivation store (encodeUtf8 drvPath)
  retrieveDerivationInfo' drvPath drv

retrieveDerivationInfo' :: Text -> ForeignPtr Derivation -> IO DerivationInfo
retrieveDerivationInfo' drvPath drv = do
  sourcePaths <- getDerivationSources drv
  inputDrvPaths <- getDerivationInputs drv
  outputs <- getDerivationOutputs drv
  env <- getDerivationEnv drv
  platform <- getDerivationPlatform drv
  let requiredSystemFeatures = maybe [] splitFeatures $ M.lookup "requiredSystemFeatures" env
      splitFeatures = filter (not . T.null) . T.split (== ' ') . decode
      decode = decodeUtf8With lenientDecode
  pure $
    DerivationInfo
      { derivationPath = drvPath,
        platform = decode platform,
        requiredSystemFeatures = requiredSystemFeatures,
        inputDerivations = inputDrvPaths & map (bimap decode (map decode)) & M.fromList,
        inputSources = map decode sourcePaths,
        outputs =
          outputs
            & map
              ( \output ->
                  ( decode $ derivationOutputName output,
                    DerivationInfo.OutputInfo
                      { DerivationInfo.path = decode $ derivationOutputPath output,
                        DerivationInfo.isFixed = derivationOutputHashAlgo output /= ""
                      }
                  )
              )
            & M.fromList
      }
