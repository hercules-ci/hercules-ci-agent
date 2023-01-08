{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Hercules.API.Forge where

import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude
import Servant.API
import Servant.API.Generic

data ForgeResourceGroup auth f = ForgeResourceGroup
  { get ::
      f
        :- Summary ("Get the forge.")
          :> auth
          :> Get '[JSON] Forge,
    delete ::
      f
        :- Summary ("Delete the forge.")
          :> auth
          :> Delete '[JSON] NoContent
  }
  deriving (Generic)

data ForgeAPI auth f = ForgeAPI
  { forgeById ::
      f
        :- Substitute
             ("forges" :> Capture "forgeId" (Id Forge) :> Placeholder)
             (ToServantApi (ForgeResourceGroup auth)),
    forgeByName ::
      f
        :- Substitute
             ( "forge"
                 :> Capture "forgeName" (Name Forge)
                 :> Placeholder
             )
             (ToServantApi (ForgeResourceGroup auth))
  }
  deriving (Generic)
