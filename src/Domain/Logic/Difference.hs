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
module Domain.Logic.Difference (difference) where

import Common.Rewriting
import Common.Uniplate
import Domain.Logic.Formula
import Data.Maybe
import Common.Rewriting.TreeDiff
import Domain.Logic.Generator()

test = treeDiff ((Var "p" :||: (Var "q" :||: Var "s") :||: Var "s"))
                  (Var "p" :||: (Var "s" :||: Var "r") :||: Var "s")

difference :: Eq a => Logic a -> Logic a -> Maybe (Logic a, Logic a)
difference p q 
   | shallowEq p q =
        case p of
           _ :||: _ ->
              let ps = disjunctions p
                  qs = disjunctions q
              in differenceAssoc (:||:) (ps, qs)
           _ :&&: _ ->
              let ps = conjunctions p
                  qs = conjunctions q
              in differenceAssoc (:&&:) (ps, qs)
           _ -> differenceList (children p) (children q)
   | otherwise = Just (p, q)
   
differenceList :: Eq a => [Logic a] -> [Logic a] -> Maybe (Logic a, Logic a)
differenceList xs ys
   | length xs /= length ys = Nothing
   | otherwise = 
        case catMaybes (zipWith difference xs ys) of
           [p] -> Just p
           _   -> Nothing
           
differenceAssoc :: Eq a => (Logic a -> Logic a -> Logic a) -> 
                           ([Logic a], [Logic a]) -> Maybe (Logic a, Logic a)
differenceAssoc op = make . rev . f . rev . f 
 where
   f (p:ps, q:qs) | not (null ps || null qs || isJust (difference p q)) = 
      f (ps, qs)
   f pair = pair
   
   rev  (ps, qs) = (reverse ps, reverse qs)
   make pair = 
      case pair of 
         ([p], [q]) -> difference p q
         (ps, qs)   -> Just (foldr1 op ps, foldr1 op qs)