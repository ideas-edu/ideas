-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
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

module Ideas.Common.Rewriting.Difference
   ( difference, differenceEqual
   , differenceWith, differenceEqualWith
   ) where

import Control.Monad
import Data.Function
import Data.Maybe
import Ideas.Common.Rewriting.Term
import Ideas.Common.View

differenceWith :: View Term a -> a -> a -> Maybe (a, a)
differenceWith = diff (\_ _ -> True)

differenceEqualWith :: View Term a -> (a -> a -> Bool) -> a -> a -> Maybe (a, a)
differenceEqualWith v eq p q = guard (eq p q) >> diff eq v p q

difference :: IsTerm a => a -> a -> Maybe (a, a)
difference = diff (\_ _ -> True) termView

-- | This function returns the difference, except that the
-- returned terms should be logically equivalent. Nothing can signal that
-- there is no difference, or that the terms to start with are not equivalent.
differenceEqual :: IsTerm a => (a -> a -> Bool) -> a -> a -> Maybe (a, a)
differenceEqual eq p q = do
   guard (eq p q)
   diff eq termView p q

collectSym :: Symbol -> Term -> [Term]
collectSym s a = maybe [a] (uncurry ((++) `on` collectSym s)) (isBinary s a)

-- local implementation function
diff :: (a -> a -> Bool) -> View Term a -> a -> a -> Maybe (a, a)
diff eq v a b = do
   let eqT x y = fromMaybe False $ liftM2 eq (match v x) (match v y)
   (t1, t2) <- diffTerm eqT (build v a) (build v b)
   liftM2 (,) (match v t1) (match v t2)

diffTerm :: (Term -> Term -> Bool) -> Term -> Term -> Maybe (Term, Term)
diffTerm eq = rec
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

   diffA s = curry (make . rev . f . rev . f)
    where
      f (p:ps, q:qs) | not (null ps || null qs) &&
                       isNothing (rec p q) &&
                       equal ps qs =
         f (ps, qs)
      f pair = pair

      equal     = eq `on` builder
      rev       = reverse *** reverse
      builder   = foldr1 (binary s)
      make pair =
         case pair of
            ([p], [q]) -> rec p q
            (ps, qs)   -> Just (builder ps, builder qs)