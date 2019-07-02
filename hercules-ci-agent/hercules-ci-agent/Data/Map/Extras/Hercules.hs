module Data.Map.Extras.Hercules where

import           Protolude               hiding ( groupBy )

import           Control.Arrow                  ( (&&&) )
import qualified Data.Map                      as M
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NEL

groupOn :: Ord k => (a -> k) -> [a] -> M.Map k [a]
groupOn f = fmap toList . groupOnNEL f

groupOnNEL :: Ord k => (a -> k) -> [a] -> M.Map k (NEL.NonEmpty a)
groupOnNEL f =
  M.fromList
    . map (fst . NEL.head &&& map snd)
    . NEL.groupBy ((==) `on` fst)
    . L.sortBy (compare `on` fst)
    . map (f &&& identity)

