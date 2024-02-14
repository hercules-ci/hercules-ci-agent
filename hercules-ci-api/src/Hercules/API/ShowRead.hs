module Hercules.API.ShowRead
  ( ShowRead (ShowRead),
  )
where

import Data.Either (Either (Right))
import Data.Text qualified as T
import Web.HttpApiData (FromHttpApiData (parseQueryParam), ToHttpApiData)
import Web.Internal.HttpApiData (ToHttpApiData (toUrlPiece))
import Prelude (Either (Left), Read, Show (show), reads)

-- | A newtype wrapper for using 'Show' and 'Read' instances with @DerivingVia@.
newtype ShowRead a = ShowRead a

instance (Show a) => ToHttpApiData (ShowRead a) where
  toUrlPiece (ShowRead a) = toUrlPiece (T.pack (show a))

instance (Read a) => FromHttpApiData (ShowRead a) where
  parseQueryParam t =
    case reads (T.unpack t) of
      [(a, "")] -> Right (ShowRead a)
      _ -> Left "Could not parse"
