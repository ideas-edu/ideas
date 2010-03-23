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
-- Compute the difference of two terms generically, taking associativity
-- into account.
--
-----------------------------------------------------------------------------
module Common.Rewriting.Difference 
   ( difference, differenceEqual, differenceMode
   ) where

import Common.Rewriting.AC
import Common.Rewriting.RewriteRule
import Control.Monad
import Common.Uniplate
import Data.Maybe

differenceMode :: Rewrite a => (a -> a -> Bool) -> Bool -> a -> a -> Maybe (a, a)
differenceMode eq b =
   if b then differenceEqual eq else difference

-- | This function returns the difference, except that the 
-- returned terms should be logically equivalent. Nothing can signal that
-- there is no difference, or that the terms to start with are not equivalent.
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
