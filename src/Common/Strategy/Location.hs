-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Locations in a strategy
--
-----------------------------------------------------------------------------
module Common.Strategy.Location
   ( subTaskLocation, nextTaskLocation
   , strategyLocations, subStrategy
   ) where

import Common.Id
import Common.Strategy.Abstract
import Common.Strategy.Core
import Common.Utils.Uniplate
import Data.Maybe

-----------------------------------------------------------
--- Strategy locations

-- old (current) and actual (next major rule) location
subTaskLocation :: LabeledStrategy a -> Id -> Id -> Id
subTaskLocation s xs ys = g (rec (f xs) (f ys))
 where
   f = fromMaybe [] . toLoc s
   g = fromMaybe (getId s) . fromLoc s
   rec (i:is) (j:js)
      | i == j    = i : rec is js
      | otherwise = []
   rec _ (j:_)    = [j]
   rec _ _        = []

-- old (current) and actual (next major rule) location
nextTaskLocation :: LabeledStrategy a -> Id -> Id -> Id
nextTaskLocation s xs ys = g (rec (f xs) (f ys))
 where
   f = fromMaybe [] . toLoc s
   g = fromMaybe (getId s) . fromLoc s
   rec (i:is) (j:js)
      | i == j    = i : rec is js
      | otherwise = [j]
   rec _ _        = []

-- | Returns a list of all strategy locations, paired with the labeled
-- substrategy at that location
strategyLocations :: LabeledStrategy a -> [([Int], LabeledStrategy a)]
strategyLocations s = ([], s) : rec [] (toCore (unlabel s))
 where
   rec is = concat . zipWith make (map (:is) [0..]) . collect

   make is (l, core) =
      let ls  = makeLabeledStrategy l (fromCore core)
      in (is, ls) : rec is core

   collect core =
      case core of
         Label l t -> [(l, t)]
         Not _     -> []
         _         -> concatMap collect (children core)

-- | Returns the substrategy or rule at a strategy location. Nothing
-- indicates that the location is invalid
subStrategy :: Id -> LabeledStrategy a -> Maybe (LabeledStrategy a)
subStrategy loc =
   fmap snd . listToMaybe . filter ((==loc) . getId . snd) . strategyLocations

fromLoc :: LabeledStrategy a -> [Int] -> Maybe Id
fromLoc s loc = fmap getId (lookup loc (strategyLocations s))

toLoc :: LabeledStrategy a -> Id -> Maybe [Int]
toLoc s i =
   fmap fst (listToMaybe (filter ((==i) . getId . snd) (strategyLocations s)))