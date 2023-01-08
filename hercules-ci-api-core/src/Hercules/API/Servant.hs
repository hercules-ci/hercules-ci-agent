{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extras for working with servant
module Hercules.API.Servant
  ( noContent,

    -- * 'Generic'
    useApi,
    useApiE,
    enterApi,
    enterApiE,

    -- * Substitution
    Substitute,
    Placeholder,
  )
where

import Control.Monad (void)
import Data.Kind (Type)
import Servant.API
import Servant.API.Generic
import Prelude

-- | Postcomposes 'Servant.API.Generic.fromServant' to an accessor,
-- preserving the mode parameter, because otherwise the mode parameter
-- can not be inferred.
--
-- Ideally, this functionality would be built into a new combinator.
useApi ::
  forall subapi api mode.
  (GenericServant subapi mode) =>
  (api mode -> ToServant subapi mode) ->
  api mode ->
  subapi mode
useApi = (Servant.API.Generic.fromServant .)

-- | Like 'useApi' but constrains the @auth@ type variable that's passed to
-- subapis.
useApiE ::
  forall subapi api mode a.
  (GenericServant (subapi a) mode) =>
  (api a mode -> ToServant (subapi a) mode) ->
  api a mode ->
  subapi a mode
useApiE = useApi

-- | @flip 'useApi'
enterApi ::
  forall subapi api mode.
  (GenericServant subapi mode) =>
  api mode ->
  (api mode -> ToServant subapi mode) ->
  subapi mode
enterApi = flip useApi

-- | @flip 'useApiE'
enterApiE ::
  forall subapi api mode a.
  (GenericServant (subapi a) mode) =>
  api a mode ->
  (api a mode -> ToServant (subapi a) mode) ->
  subapi a mode
enterApiE = flip useApi

-- | 'Control.Monad.void' specialised to 'NoContent' to soothe the
-- compiler that rightfully warns about throwing away a do notation
-- result. By specialising, we make sure that we still get warnings
-- if the result type changes in the future. (We'll get an error)
noContent :: Functor m => m Servant.API.NoContent -> m ()
noContent = void

-- | A reference to the @subapi@ parameter in 'Substitute'
data Placeholder

-- | Replaces 'Placeholder' by @subapi@ in the API @target@.
type family Substitute (target :: k) subapi :: Type

type instance Substitute Placeholder subapi = subapi

type instance Substitute (a :> b) subapi = a :> Substitute b subapi

type instance Substitute (a :<|> b) subapi = Substitute a subapi :<|> Substitute b subapi
