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
-- Compute the difference of two logical propositions, taking associativity
-- into account. Ideally, we would reuse treeDiff (see Common.Rewriting), 
-- but the current implementation does not use associativity.
--
-----------------------------------------------------------------------------
module Domain.Logic.Difference (difference, differenceEqual) where

import Control.Monad
import Common.Rewriting
import Common.Uniplate
import Domain.Logic.Formula
import Data.Maybe
import Domain.Logic.Generator()

--test = treeDiff ((Var "p" :||: (Var "q" :||: Var "s") :||: Var "s"))
--                  (Var "p" :||: (Var "s" :||: Var "r") :||: Var "s")

-- Workaround: this function returns the diff, except that the 
-- returned propositions should be logically equivalent. Currently
-- used for reporting where a rewrite rule was applied.
differenceEqual :: SLogic -> SLogic -> Maybe (SLogic, SLogic)
differenceEqual p q = do
   guard (eqLogic p q)
   diff True p q

difference :: SLogic -> SLogic -> Maybe (SLogic, SLogic)
difference = diff False

-- local implementation function
diff :: Bool -> SLogic -> SLogic -> Maybe (SLogic, SLogic)
diff eqOpt p q 
   | shallowEq p q =
        case p of
           _ :||: _ ->
              let ps = disjunctions p
                  qs = disjunctions q
              in diffA eqOpt (:||:) (ps, qs)
           _ :&&: _ ->
              let ps = conjunctions p
                  qs = conjunctions q
              in diffA eqOpt (:&&:) (ps, qs)
           _ -> diffList eqOpt (children p) (children q)
   | otherwise = Just (p, q)
   
diffList :: Bool -> [SLogic] -> [SLogic] -> Maybe (SLogic, SLogic)
diffList eqOpt xs ys
   | length xs /= length ys = Nothing
   | otherwise = 
        case catMaybes (zipWith (diff eqOpt) xs ys) of
           [p] -> Just p
           _   -> Nothing
           
diffA :: Bool -> (SLogic -> SLogic -> SLogic) -> 
         ([SLogic], [SLogic]) -> Maybe (SLogic, SLogic)
diffA eqOpt op = make . rev . f . rev . f 
 where
   f (p:ps, q:qs) | not (null ps || null qs) && 
                    isNothing (diff eqOpt p q) && 
                    equal ps qs = 
      f (ps, qs)
   f pair = pair
   
   equal ps qs = eqLogic (foldr1 op ps) (foldr1 op qs)
   rev  (ps, qs) = (reverse ps, reverse qs)
   make pair = 
      case pair of 
         ([p], [q]) -> diff eqOpt p q
         (ps, qs)   -> Just (foldr1 op ps, foldr1 op qs)