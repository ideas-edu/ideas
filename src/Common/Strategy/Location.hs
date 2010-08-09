-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
   ( StrategyLocation, topLocation, nextLocation, downLocation
   , locationDepth
   , subTaskLocation, nextTaskLocation, parseStrategyLocation
   , StrategyOrRule, strategyLocations, subStrategy, addLocation
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Core
import Common.Transformation
import Common.Uniplate
import Common.Utils (readM)
import Data.Foldable (toList)
import Data.Sequence hiding (take, reverse)

-----------------------------------------------------------
--- Strategy locations

-- | A strategy location corresponds to a substrategy or a rule
newtype StrategyLocation = SL (Seq Int)
   deriving Eq

instance Show StrategyLocation where
   show (SL xs) = show (toList xs)

type StrategyOrRule a = Either (LabeledStrategy a) (Rule a)

topLocation :: StrategyLocation 
topLocation = SL empty

nextLocation :: StrategyLocation -> StrategyLocation
nextLocation (SL xs) =
   case viewr xs of
      EmptyR  -> topLocation -- invalid
      ys :> a -> SL (ys |> (a+1))

downLocation :: StrategyLocation -> StrategyLocation
downLocation (SL xs) = SL (xs |> 0)

locationDepth :: StrategyLocation -> Int
locationDepth (SL xs) = Data.Sequence.length xs

-- old (current) and actual (next major rule) location
subTaskLocation :: StrategyLocation -> StrategyLocation -> StrategyLocation
subTaskLocation (SL xs) (SL ys) = SL (rec xs ys)
 where
   rec xs ys =
      case (viewl xs, viewl ys) of
         (i :< is, j :< js) 
            | i == j    -> i <| rec is js 
            | otherwise -> empty
         (_, j :< _)    -> singleton j
         _              -> empty

-- old (current) and actual (next major rule) location
nextTaskLocation :: StrategyLocation -> StrategyLocation -> StrategyLocation
nextTaskLocation (SL xs) (SL ys) = SL (rec xs ys)
 where
   rec xs ys =
      case (viewl xs, viewl ys) of
         (i :< is, j :< js)
            | i == j    -> i <| rec is js
            | otherwise -> singleton j
         _              -> empty

parseStrategyLocation :: String -> Maybe StrategyLocation
parseStrategyLocation = fmap (SL . fromList) . readM

-- | Returns a list of all strategy locations, paired with the labeled 
-- substrategy or rule at that location

strategyLocations :: LabeledStrategy a -> [(StrategyLocation, StrategyOrRule a)]
strategyLocations = collect . addLocation . toCore . toStrategy
 where
   collect core = 
      case core of
         Label (loc, info) s -> 
            let this = makeLabeledStrategy info (mapLabel snd s)
            in (loc, Left this) : collect s
         Not _ -> []
         _     -> concatMap collect (children core)

-- | Returns the substrategy or rule at a strategy location. Nothing 
-- indicates that the location is invalid
subStrategy :: StrategyLocation -> LabeledStrategy a -> Maybe (StrategyOrRule a)
subStrategy loc = lookup loc . strategyLocations
            
-- local helper functions that decorates interesting places with a 
-- strategy lcations (major rules, and labels)
addLocation :: Core l a -> Core (StrategyLocation, l) a
addLocation = snd . rec topLocation . mapLabel (\l -> (error "no location", l))
 where
   rec loc core =
      case core of
         Label (_, l) a -> 
            let new = snd (rec (downLocation loc) a)
            in (nextLocation loc, Label (loc, l) new)
         Not a -> (loc, Not (noLabels a))
         _ -> 
            let (cs, f) = uniplate core
                (l, xs) = foldl op (loc, [])  cs
                op (l1, as) x = let (l2, a) = rec l1 x in (l2, a:as)
            in (l, f (reverse xs))