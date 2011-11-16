-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( unify, match, matchExtended
   , unificationTests
   ) where

import Common.Rewriting.AC (pairingsA)
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Utils.TestSuite
import Control.Arrow
import Control.Monad

-----------------------------------------------------------
-- Unification (in both ways)

unify :: Term -> Term -> Maybe Substitution
unify term1 term2 =
   case (term1, term2) of
      (TMeta i, TMeta j) | i == j ->
         return emptySubst
      (TMeta i, _) | not (i `hasMetaVar` term2) ->
         return (singletonSubst i term2)
      (_, TMeta j) | not (j `hasMetaVar` term1) ->
         return (singletonSubst j term1)
      (TApp f a, TApp g b) -> do
         s1 <- unify f g
         s2 <- unify (s1 |-> a) (s1 |-> b)
         return (s2 @@ s1)
      _ | term1 == term2 ->
         return emptySubst
      _ -> Nothing

match :: MonadPlus m => Term -> Term -> m Substitution
match term1 term2 =
   case (term1, term2) of
      (TMeta i, TMeta j) | i == j ->
         return emptySubst
      (TMeta i, _) | not (i `hasMetaVar` term2) ->
         return (singletonSubst i term2)
      (_, TMeta _) ->
         fail "unifyM: no unifier"
      (TApp f a, TApp g b) -> do
         s1 <- match f g
         s2 <- match (s1 |-> a) b
         guard (composable s1 s2)
         return (s1 @@ s2)
      _ | term1 == term2 ->
         return emptySubst
      _ -> fail "unifyM: no unifier"

-----------------------------------------------------------
-- Matching (or: one-way unification)

-- If the top-level symbol (of the left-hand side) is an associative binary 
-- operator, extend both sides optionally with a meta-variable.
matchExtended :: Term -> Term -> [(Substitution, Maybe Term, Maybe Term)]
matchExtended x y = 
   [ (sub, lookupVar mvLeft sub, lookupVar mvRight sub) 
   | f   <- extensions
   , sub <- matchA (f x) y 
   ]
 where
   mvLeft     = nextMetaVar x
   mvRight    = mvLeft + 1
   extensions = 
      case getSpine x of
         (TCon s, [_, _]) | isAssociative s -> 
            let extLeft  = binary s (TMeta mvLeft)
                extRight = flip (binary s) (TMeta mvRight)
            in [ f . g | f <- [id, extLeft], g <- [id, extRight] ]
         _ -> [id]

-- second term should not have meta variables
matchA :: Term -> Term -> [Substitution]
matchA = rec
 where
   rec (TMeta i) y =
      return (singletonSubst i y)

   rec x y =
      case getSpine x of
         (TCon s, [a1, a2]) | isAssociative s ->
            concatMap (uncurry recList . unzip) (associativeMatch s a1 a2 y)
            -- | s == newSymbol "view.plus" -> matchComPlus [a1, a2] y
         (a, as) -> do
            let (b, bs) = getSpine y
            guard (a == b)
            recList as bs

   recList [] [] = return emptySubst
   recList (x:xs) (y:ys) = do
      s1 <- rec x y
      s2 <- recList (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   recList _ _ = []
   
   {-
   matchComPlus :: [Term] -> Term -> [Substitution]
   matchComPlus [a1, a2] y = 
      let (.+) = binary (newSymbol "arith1.plus") 
      in rec False (a1 .+ a2) y ++ rec False (a2 .+ a1) y
   matchComPlus _ _ = [] -}


associativeMatch :: Symbol -> Term -> Term -> Term -> [[(Term, Term)]]
associativeMatch s1 a1 a2 (TApp (TApp (TCon s2) b1) b2)
   | s1==s2 = map (map make) result
 where
   as = collect a1 . collect a2 $ []
   bs = collect b1 . collect b2 $ []

   result = pairingsA True as bs
   make   = construct *** construct

   collect term =
      case getFunction term of
         Just (t, [a, b]) | s1==t -> collect a . collect b
         _ -> (term:)

   construct xs
      | null xs   = error "associativeMatcher: empty list"
      | otherwise = foldr1 (binary s1) xs
associativeMatch _ _ _ _ = []

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