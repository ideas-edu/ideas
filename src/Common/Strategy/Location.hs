-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
   ( StrategyLocation, StrategyOrRule
   , strategyLocations, subStrategy, addLocation
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Core
import Common.Transformation

-----------------------------------------------------------
--- Strategy locations

-- | A strategy location corresponds to a substrategy or a rule
type StrategyLocation = [Int]

type StrategyOrRule a = Either (LabeledStrategy a) (Rule a)

-- | Returns a list of all strategy locations, paired with the labeled 
-- substrategy or rule at that location

strategyLocations :: LabeledStrategy a -> [(StrategyLocation, Either (LabeledStrategy a) (Rule a))]
strategyLocations = rec [] 
 where
   rec loc ls = 
      let f i (Left (l,core)) = rec (loc++[i]) (label l core)
          f i (Right r)       = [(loc++[i], Right r)]
      in (loc, Left ls) : concat (zipWith f [0..] (collectS (toCore (unlabel ls))))

-- | Returns the substrategy or rule at a strategy location. Nothing indicates that the location is invalid
subStrategy :: StrategyLocation -> LabeledStrategy a -> Maybe (StrategyOrRule a)
subStrategy loc ls =
   case loc of
      []   -> return (Left ls) 
      n:ns -> 
         case drop n (collectS (toCore (unlabel ls))) of
            Left (l, core):_ -> subStrategy ns (label l core)
            Right r:_ | null ns -> Just (Right r)
            _ -> Nothing
            
addLocation :: Core l a -> Core (StrategyLocation, l) a
addLocation = fst . ($ 0) . rec []
 where
   rec loc core =
      case core of
         Label l a -> \i -> let here = loc++[i]
                                pair = (here, l)
                                rest = fst (rec here a 0)
                            in (Label pair rest, i+1)
         a :*: b   -> lift2 (:*:)  a b
         a :|: b   -> lift2 (:|:)  a b
         a :|>: b  -> lift2 (:|>:) a b
         Many a    -> lift1 Many a
         Rec n a   -> lift1 (Rec n) a
         _         -> \i -> (noLabels core, i) -- including Not
    where
      lift1 f a i = 
         let (na, i1) = rec loc a i
         in (f na, i1)
      lift2 f a b i = 
         let (na, i1) = rec loc a i
             (nb, i2) = rec loc b i1
         in (f na nb, i2)