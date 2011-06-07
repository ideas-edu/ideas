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
-----------------------------------------------------------------------------
module Common.Rewriting.Unification 
   ( unify, match, matchA, specialLeft, specialRight
   , unificationTests
   ) where

import Common.Rewriting.Term
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Control.Arrow
import Control.Monad
import Common.TestSuite

-----------------------------------------------------------
-- Unification (in both ways)

unify :: Term -> Term -> Maybe Substitution
unify term1 term2 = 
   case (term1, term2) of
      (Meta i, Meta j) | i == j -> 
         return emptySubst
      (Meta i, _) | not (i `hasMetaVar` term2) -> 
         return (singletonSubst i term2)
      (_, Meta j) | not (j `hasMetaVar` term1) -> 
         return (singletonSubst j term1)
      (Apply f a, Apply g b) -> do
         s1 <- unify f g
         s2 <- unify (s1 |-> a) (s1 |-> b)
         return (s2 @@ s1)
      _ | term1 == term2 -> 
         return emptySubst
      _ -> Nothing

match :: MonadPlus m => Term -> Term -> m Substitution
match term1 term2 = 
   case (term1, term2) of
      (Meta i, Meta j) | i == j -> 
         return emptySubst
      (Meta i, _) | not (i `hasMetaVar` term2) -> 
         return (singletonSubst i term2)
      (_, Meta _) -> 
         fail "unifyM: no unifier"
      (Apply f a, Apply g b) -> do
         s1 <- match f g
         s2 <- match (s1 |-> a) b
         guard (composable s1 s2)
         return (s1 @@ s2)
      _ | term1 == term2 -> 
         return emptySubst
      _ -> fail "unifyM: no unifier"

-----------------------------------------------------------
-- Matching (or: one-way unification)

-- second term should not have meta variables

matchA :: [Symbol] -> Term -> Term -> [Substitution]
matchA assocSymbols = rec True
 where
   rec _ (Meta i) y = 
      return (singletonSubst i y)

   rec isTop x y =
      case getSpine x of
         (Con s, [a1, a2]) | s `elem` assocSymbols ->
            concatMap (uncurry recList . unzip) (associativeMatch isTop s a1 a2 y)
         (a, as) -> do
            let (b, bs) = getSpine y
            guard (a == b)
            recList as bs

   recList [] [] = return emptySubst
   recList (x:xs) (y:ys) = do
      s1 <- rec False x y
      s2 <- recList (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   recList _ _ = []
      
associativeMatch :: Bool -> Symbol -> Term -> Term -> Term -> [[(Term, Term)]]
associativeMatch isTop s1 a1 a2 (Apply (Apply (Con s2) b1) b2) 
   | s1==s2 = map (map make) result
 where
   as = collect a1 . collect a2 $ []
   bs = collect b1 . collect b2 $ []
   list | isTop     = map ($ as) [id, extLeft, extRight, extBoth]
        | otherwise = [as]
        
   extLeft  = (Meta specialLeft:)
   extRight = (++[Meta specialRight])
   extBoth  = extLeft . extRight
   
   result = concatMap (\zs -> pairingsA True zs bs) list
   make   = construct *** construct
   
   collect term =
      case getFunction term of
         Just (t, [a, b]) | s1==t -> collect a . collect b
         _ -> (term:)
   
   construct xs 
      | null xs   = error "associativeMatcher: empty list"
      | otherwise = foldr1 (binary s1) xs
associativeMatch _ _ _ _ _ = []

specialLeft, specialRight :: Int -- special meta variables for context extension
specialLeft  = maxBound
specialRight = pred specialLeft

-----------------------------------------------------------
--- * Test unification properties

unificationTests :: TestSuite
unificationTests = suite "Unification" $ do
   addProperty "unify" $ \a b ->
      case unify a b of
         Just s  -> (s |-> a) == (s |-> b)
         Nothing -> True
   addProperty "unify-succeed" $ \a s -> 
      let b = s |-> a in
      case unify a b of
         Just s2 -> (s2 |-> a) == (s2 |-> b)
         Nothing -> False
   addProperty "match" $ \a b -> 
      case match a b of
         Just s  -> (s |-> a) == b
         Nothing -> True
   addProperty "match-succeed" $ \a s -> 
      let b = s |-> a in
      case match a (s |-> a) of
         Just s2 -> (s2 |-> a) == b
         Nothing -> True
