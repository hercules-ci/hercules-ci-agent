module Data.Map.Extras.Hercules where

import Control.Arrow ((&&&))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Protolude hiding (groupBy)

groupOn :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
groupOn f = fmap toList . groupOnNEL f

groupOnNEL :: (Ord k) => (a -> k) -> [a] -> M.Map k (NEL.NonEmpty a)
groupOnNEL f =
  M.fromList
    . map (fst . NEL.head &&& map snd)
    . NEL.groupBy ((==) `on` fst)
    . L.sortBy (compare `on` fst)
    . map (f &&& identity)
