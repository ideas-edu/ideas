-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
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

-- returns a result if the terms are different
diffTerm :: Term -> Term -> Maybe (Term, Term)
diffTerm p q =
   case (getFunctionA p, getFunctionA q) of
      (Just (s1, ps), Just (s2, qs))
         | s1 == s2  -> diffList ps qs
         | otherwise -> here
      _  | p == q    -> Nothing
         | otherwise -> here
 where
   here = Just (p, q)

   diffList xs ys
      | length xs /= length ys = here
      | otherwise =
           case catMaybes (zipWith diffTerm xs ys) of
              []    -> Nothing
              [one] -> Just one
              _     -> here

getFunctionA :: WithFunctions a => a -> Maybe (Symbol, [a])
getFunctionA a = f <$> getFunction a
 where
   f (s, xs) = (s, if isAssociative s then collectSym s a else xs)

collectSym :: WithFunctions a => Symbol -> a -> [a]
collectSym s a = maybe [a] (uncurry ((++) `on` collectSym s)) (isBinary s a)