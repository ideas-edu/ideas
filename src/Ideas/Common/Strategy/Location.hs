-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Common.Strategy.Location
   ( checkLocation, subTaskLocation, nextTaskLocation
   , strategyLocations
   ) where

import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Core
import Ideas.Common.Utils.Uniplate

-----------------------------------------------------------
--- Strategy locations

checkLocation :: Id -> LabeledStrategy a -> Bool
checkLocation loc =
   any ((==loc) . getId . snd) . strategyLocations

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

-- | Returns a list of all strategy locations, paired with the label
strategyLocations :: LabeledStrategy a -> [([Int], Id)]
strategyLocations s = ([], getId s) : rec [] (toCore (unlabel s))
 where
   rec is = concat . zipWith make (map (:is) [0..]) . collect

   make is (l, mc) = (is, l) : maybe [] (rec is) mc

   collect core =
      case core of
         Label l c -> [(l, Just c)]
         Sym r | isMajor r -> [(getId r, Nothing)]
         _         -> concatMap collect (children core)

fromLoc :: LabeledStrategy a -> [Int] -> Maybe Id
fromLoc s loc = fmap getId (lookup loc (strategyLocations s))

toLoc :: LabeledStrategy a -> Id -> Maybe [Int]
toLoc s i =
   fmap fst (listToMaybe (filter ((==i) . getId . snd) (strategyLocations s)))