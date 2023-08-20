module Hercules.Agent.Store where

import Hercules.Agent.WorkerProtocol.OutputInfo (OutputInfo (OutputInfo))
import Hercules.Agent.WorkerProtocol.OutputInfo qualified
import Hercules.CNix.Store (DerivationOutput (derivationOutputName, derivationOutputPath), Store, getStorePathBaseName, queryPathInfo, validPathInfoNarHash32, validPathInfoNarSize, validPathInfoReferences')
import Hercules.CNix.Store qualified as CNix
import Protolude

toDrvInfo :: Store -> DerivationOutput -> IO OutputInfo
toDrvInfo store drvOut = do
  -- FIXME: ca-derivations: always get the built path
  vpi <- for (derivationOutputPath drvOut) (queryPathInfo store)
  hash_ <- traverse validPathInfoNarHash32 vpi
  path <- traverse (CNix.storePathToPath store) (derivationOutputPath drvOut)
  let size = fmap validPathInfoNarSize vpi
  refs <- traverse (traverse getStorePathBaseName <=< validPathInfoReferences') vpi
  pure
    OutputInfo
      { name = derivationOutputName drvOut,
        path = fromMaybe "" path,
        hash = fromMaybe "" hash_,
        size = fromMaybe 0 size,
        references = fromMaybe [] refs
      }
