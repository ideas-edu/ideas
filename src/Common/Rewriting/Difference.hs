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
   ( difference, differenceEqual, differenceMode
   ) where

import Common.Rewriting.Operator
import Common.Rewriting.Term
import Common.Rewriting.RewriteRule
import Common.Utils (safeHead)
import Control.Monad
import Common.Uniplate
import Data.Maybe

differenceMode :: (Rewrite a, Uniplate a) 
               => (a -> a -> Bool) -> Bool -> a -> a -> Maybe (a, a)
differenceMode eq b =
   if b then differenceEqual eq else difference

-- | This function returns the difference, except that the 
-- returned terms should be logically equivalent. Nothing can signal that
-- there is no difference, or that the terms to start with are not equivalent.
differenceEqual :: (Rewrite a, Uniplate a) 
                => (a -> a -> Bool) -> a -> a -> Maybe (a, a)
differenceEqual eq p q = do
   guard (eq p q)
   diff eq p q

difference :: (Rewrite a, Uniplate a) => a -> a -> Maybe (a, a)
difference = diff (\_ _ -> True)

shallowEq :: IsTerm a => a -> a -> Bool
shallowEq a b = 
   let f  = liftM fst . getFunction
       ta = toTerm a
       tb = toTerm b 
   in fromMaybe (ta == tb) $ liftM2 (==) (f ta) (f tb)

findOperator :: (IsTerm a, Uniplate a) => a -> [BinaryOp a] -> Maybe (BinaryOp a)
findOperator a = safeHead . filter (`isBinaryOp` a)

collectOperator :: BinaryOp a -> a -> [a]
collectOperator f a =
   case binaryOpMatch f a of
      Just (x, y) -> collectOperator f x ++ collectOperator f y
      Nothing     -> [a]

-- local implementation function
diff :: (Rewrite a, Uniplate a) => (a -> a -> Bool) -> a -> a -> Maybe (a, a)
diff eq = rec
 where
   rec p q
      | shallowEq p q =
           case findOperator p associativeOps of
              Just op ->
                 let ps = collectOperator op p
                     qs = collectOperator op q
                 in diffA op ps qs
              _ -> diffList (children p) (children q)
      | otherwise = Just (p, q)

   diffList xs ys
      | length xs /= length ys = Nothing
      | otherwise = 
           case catMaybes (zipWith rec xs ys) of
              [p] -> Just p
              _   -> Nothing
           
   diffA op = curry (make . uncurry rev . f . uncurry rev . f)
    where
      f (p:ps, q:qs) | not (null ps || null qs) && 
                       isNothing (rec p q) && 
                       equal ps qs = 
         f (ps, qs)
      f pair = pair
      
      equal ps qs = builder ps `eq` builder qs
      rev   ps qs = (reverse ps, reverse qs)
      builder     = foldr1 (binaryOp op)
      make pair   = 
         case pair of 
            ([p], [q]) -> rec p q
            (ps, qs)   -> Just (builder ps, builder qs)