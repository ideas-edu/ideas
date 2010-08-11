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
   , strategyLocations, subStrategy
   , locationToId, idToLocation
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Core
import Common.Transformation
import Common.Uniplate
import Common.Utils (readM, safeHead)
import Control.Monad
import Data.Foldable (toList)
import Data.Sequence hiding (filter, take, reverse, zipWith)

-----------------------------------------------------------
--- Strategy locations

-- | A strategy location corresponds to a substrategy or a rule
newtype StrategyLocation = SL (Seq Int)
   deriving Eq

instance Show StrategyLocation where
   show (SL xs) = show (toList xs)

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

parseStrategyLocation :: Monad m => String -> m StrategyLocation
parseStrategyLocation = liftM (SL . fromList) . readM

-- | Returns a list of all strategy locations, paired with the labeled 
-- substrategy or rule at that location
strategyLocations :: LabeledStrategy a -> [(StrategyLocation, LabeledStrategy a)]
strategyLocations s = (topLocation, s) : rec [] (toCore (unlabel s))
 where 
   rec is = concat . zipWith make (map (:is) [0..]) . collect
   
   make is (l, core) = 
      let loc = SL (fromList is)
          ls  = makeLabeledStrategy l (toStrategy core)
      in (loc, ls) : rec is core
   
   collect core =
      case core of
         Label l s -> [(l, s)]
         Not _     -> []
         _         -> concatMap collect (children core)

locationToId :: LabeledStrategy a -> StrategyLocation -> Maybe Id
locationToId s loc = fmap getId (lookup loc (strategyLocations s))

idToLocation :: LabeledStrategy a -> Id -> Maybe StrategyLocation
idToLocation s i = 
   fmap fst (safeHead (filter ((==i) . getId . snd) (strategyLocations s)))

-- | Returns the substrategy or rule at a strategy location. Nothing 
-- indicates that the location is invalid
subStrategy :: StrategyLocation -> LabeledStrategy a -> Maybe (LabeledStrategy a)
subStrategy loc = lookup loc . strategyLocations