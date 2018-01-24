-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
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

module Ideas.Common.Rewriting.Difference (difference, differenceWith) where

import Control.Monad
import Data.Function
import Data.Maybe
import Ideas.Common.Rewriting.Term
import Ideas.Common.View

difference :: IsTerm a => a -> a -> Maybe (a, a)
difference = differenceWith termView

differenceWith :: View Term a -> a -> a -> Maybe (a, a)
differenceWith v a b = do
   (t1, t2) <- diffTerm (build v a) (build v b)
   liftM2 (,) (match v t1) (match v t2)

diffTerm :: Term -> Term -> Maybe (Term, Term)
diffTerm = rec
 where
   rec p q =
      case (getFunction p, getFunction q) of
         (Just (s1, ps), Just (s2, qs))
            | s1 /= s2         -> Just (p, q)
            | isAssociative s1 -> (diffA s1 `on` collectSym s1) p q
            | otherwise        -> diffList ps qs
         _  | p == q           -> Nothing
            | otherwise        -> Just (p, q)

   diffList xs ys
      | length xs /= length ys = Nothing
      | otherwise =
           case catMaybes (zipWith rec xs ys) of
              [p] -> Just p
              _   -> Nothing

   diffA s list1 list2 = curry (make . rev . f . rev . f) list1 list2
    where
      f (p:ps, q:qs) | not (null ps || null qs) &&
                       isNothing (rec p q) =
         f (ps, qs)
      f pair = pair

      rev       = reverse *** reverse
      builder   = foldr1 (binary s)
      make pair =
         case pair of
            ([p], [q]) -> rec p q
            (_, _)     -> Just (builder list1, builder list2)
            
collectSym :: Symbol -> Term -> [Term]
collectSym s a = maybe [a] (uncurry ((++) `on` collectSym s)) (isBinary s a)