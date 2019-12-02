-- | Extras for working with servant
module Hercules.API.Servant where

import Control.Monad (void)
import Servant.API
import Servant.API.Generic
import Prelude

-- | Postcomposes 'Servant.API.Generic.fromServant' to an accessor,
-- preserving the mode parameter, because otherwise the mode parameter
-- can not be inferred.
--
-- Ideally, this functionality would be built into a new combinator.
useApi ::
  (GenericServant f mode, GenericServant g mode) =>
  (f mode -> ToServant g mode) ->
  f mode ->
  g mode
useApi = (Servant.API.Generic.fromServant .)

-- | 'Control.Monad.void' specialised to 'NoContent' to soothe the
-- compiler that rightfully warns about throwing away a do notation
-- result. By specialising, we make sure that we still get warnings
-- if the result type changes in the future. (We'll get an error)
noContent :: Functor m => m Servant.API.NoContent -> m ()
noContent = void
