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
-- Compute the difference of two terms generically, taking associativity
-- into account.
--
-----------------------------------------------------------------------------
module Common.Rewriting.Difference 
   ( difference, differenceEqual, differenceMode, differenceModeWith
   ) where

import Common.Rewriting.Operator
import Common.Rewriting.Term
import Common.Rewriting.RewriteRule
import Common.View
import Control.Monad
import Data.Function
import Data.Maybe

differenceModeWith :: View Term a -> (a -> a -> Bool) -> Bool -> a -> a -> Maybe (a, a)
differenceModeWith v eq mode 
   | mode      = \p q -> guard (eq p q) >> diff v [] syms eq p q
   | otherwise = diff v [] syms (\_ _ -> True)
 where -- quick hack
   syms = [ newSymbol ("logic1", "and"), newSymbol ("logic1", "or")]

differenceMode :: Rewrite a => (a -> a -> Bool) -> Bool -> a -> a -> Maybe (a, a)
differenceMode eq mode 
   | mode      = differenceEqual eq
   | otherwise = difference

-- | This function returns the difference, except that the 
-- returned terms should be logically equivalent. Nothing can signal that
-- there is no difference, or that the terms to start with are not equivalent.
differenceEqual :: Rewrite a => (a -> a -> Bool) -> a -> a -> Maybe (a, a)
differenceEqual eq p q = do
   guard (eq p q)
   diff termView associativeOps [] eq p q

difference :: Rewrite a => a -> a -> Maybe (a, a)
difference = diff termView associativeOps [] (\_ _ -> True)
      
collectSym :: Symbol -> Term -> [Term]
collectSym s a = maybe [a] (uncurry ((++) `on` collectSym s)) (isBinary s a)

-- local implementation function
diff :: View Term a -> [BinaryOp a] -> [Symbol] -> (a -> a -> Bool) -> a -> a -> Maybe (a, a)
diff v ops ss eq a b = do
   let eqT x y = fromMaybe False $ liftM2 eq (match v x) (match v y)
       syms    = map f ops ++ ss
       f op    = case getFunction (build v (binaryOp op a b)) of
                    Just (x, _) -> x
                    _ -> newSymbol () -- error
   (t1, t2) <- diffTerm syms eqT (build v a) (build v b)
   liftM2 (,) (match v t1) (match v t2)
            
diffTerm :: [Symbol] -> (Term -> Term -> Bool) -> Term -> Term -> Maybe (Term, Term)
diffTerm syms eq = rec
 where
   rec p q =
      case (getFunction p, getFunction q) of
         (Just (s1, ps), Just (s2, qs)) 
            | s1 /= s2       -> Just (p, q)
            | s1 `elem` syms -> (diffA s1 `on` collectSym s1) p q
            | otherwise      -> diffList ps qs
         _  | p == q         -> Nothing
            | otherwise      -> Just (p, q)

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