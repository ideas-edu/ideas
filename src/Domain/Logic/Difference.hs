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
import Data.Maybe

-- import Domain.Logic.Formula
-- import Domain.Logic.Generator()
-- import Domain.Logic.Parser
-- import Text.Parsing (fromRanged)

{-
Right term1 = parseLogic "(r /\\ p /\\ ~s /\\ (p || r)) || (r /\\ p /\\ s /\\ ~p /\\ ~r) || \
   \ (~r /\\ ~p /\\ ((~s /\\ (p || r)) || (s /\\ ~p /\\ ~r))) || (~q /\\ ((~s /\\ (p \
   \ || r)) || (s /\\ ~p /\\ ~r))) || (s /\\ ((~s /\\ (p || r)) || (s /\\ ~p /\\ ~r) \
   \ ))"
Right term2 = parseLogic "(r /\\ p /\\ ~s /\\ (p || r)) || (r /\\ p /\\ ~p /\\ s /\\ ~r) || (~r /\\ ~ \
   \ p /\\ ((~s /\\ (p || r)) || (s /\\ ~p /\\ ~r))) || (~q /\\ ((~s /\\ (p || r)) ||\
   \(s /\\ ~p /\\ ~r))) || (s /\\ ((~s /\\ (p || r)) || (s /\\ ~p /\\ ~r)))"

test = differenceEqual eqLogic (fromRanged term1) (fromRanged term2) -}

-- Workaround: this function returns the diff, except that the 
-- returned propositions should be logically equivalent. Currently
-- used for reporting where a rewrite rule was applied.
differenceEqual :: Rewrite a => (a -> a -> Bool) -> a -> a -> Maybe (a, a)
differenceEqual eq p q = do
   guard (eq p q)
   diff eq p q

difference :: Rewrite a => a -> a -> Maybe (a, a)
difference = diff (\_ _ -> True)

-- local implementation function
diff :: Rewrite a => (a -> a -> Bool) -> a -> a -> Maybe (a, a)
diff eq p q 
   | shallowEq p q =
        case findOperator operators p of
           Just op | isAssociative op && not (isCommutative op) -> 
              let ps = collectWithOperator op p
                  qs = collectWithOperator op q
              in diffA eq op ps qs
           _ -> diffList eq (children p) (children q)
   | otherwise = Just (p, q)

diffList :: Rewrite a => (a -> a -> Bool) -> [a] -> [a] -> Maybe (a, a)
diffList eq xs ys
   | length xs /= length ys = Nothing
   | otherwise = 
        case catMaybes (zipWith (diff eq) xs ys) of
           [p] -> Just p
           _   -> Nothing
           
diffA :: Rewrite a => (a -> a -> Bool) -> Operator a -> [a] -> [a] -> Maybe (a, a)
diffA eq op = curry (make . uncurry rev . f . uncurry rev . f)
 where
   f (p:ps, q:qs) | not (null ps || null qs) && 
                    isNothing (diff eq p q) && 
                    (equal ps qs) = 
      f (ps, qs)
   f pair = pair
   
   equal ps qs = buildWithOperator op ps `eq` buildWithOperator op qs
   rev   ps qs = (reverse ps, reverse qs)
   make pair   = 
      case pair of 
         ([p], [q]) -> diff eq p q
         (ps, qs)   -> Just (buildWithOperator op ps, buildWithOperator op qs)
