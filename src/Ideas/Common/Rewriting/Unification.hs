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
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.Unification
   ( unify, match, matchExtended, matchList
   , Match, SymbolMatch
   , unificationTests
   ) where

import Control.Monad
import Data.Maybe
import Ideas.Common.Rewriting.AC (pairingsMatchA)
import Ideas.Common.Rewriting.Substitution
import Ideas.Common.Rewriting.Term
import Ideas.Utils.TestSuite
import qualified Data.Map as M

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
      (TCon s xs, TCon t ys) | s == t ->
         rec xs ys
      (TList xs, TList ys) ->
         rec xs ys
      _ | term1 == term2 ->
         return emptySubst
      _ -> Nothing
 where
   rec [] [] = return emptySubst
   rec (x:xs) (y:ys) = do
      s1 <- unify x y
      s2 <- rec (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   rec _ _ = fail "match: no unifier"

match :: Term -> Term -> Maybe Substitution
match term1 term2 =
   case (term1, term2) of
      (TMeta i, TMeta j) | i == j ->
         Just emptySubst
      (TMeta i, _) | not (i `hasMetaVar` term2) ->
         Just (singletonSubst i term2)
      (_, TMeta _) ->
         Nothing
      (TCon s xs, TCon t ys) | s == t ->
         rec xs ys
      (TList xs, TList ys) ->
         rec xs ys
      _ | term1 == term2 ->
         Just emptySubst
      _ -> Nothing
 where
   rec [] [] = Just emptySubst
   rec (x:xs) (y:ys) = do
      s1 <- match x y
      s2 <- rec (map (s1 |->) xs) ys
      guard (composable s1 s2)
      Just (s1 @@ s2)
   rec _ _ = Nothing

-----------------------------------------------------------
-- Matching (or: one-way unification)

type Match a = a -> a -> [Substitution]
type SymbolMatch = Match Term -> [Term] -> Term -> [Substitution]

-- If the top-level symbol (of the left-hand side) is an associative binary
-- operator, extend both sides optionally with a meta-variable.
matchExtended :: M.Map Symbol SymbolMatch -> Term -> Term -> [(Substitution, Maybe Term, Maybe Term)]
matchExtended sm x y =
   [ (sub, lookupVar mvLeft sub, lookupVar mvRight sub)
   | f   <- extensions
   , sub <- matchA sm (f x) y
   ]
 where
   mvLeft     = nextMetaVar x
   mvRight    = mvLeft + 1
   extensions =
      case x of
         TCon s [_, _] | isAssociative s ->
            let extLeft  = binary s (TMeta mvLeft)
                extRight = flip (binary s) (TMeta mvRight)
            in [ f . g | f <- [id, extLeft], g <- [id, extRight] ]
         _ -> [id]

-- second term should not have meta variables
matchA :: M.Map Symbol SymbolMatch -> Match Term
matchA sm = rec
 where
   rec (TMeta i) y =
      return (singletonSubst i y)
   rec (TList xs) (TList ys) =
      matchList rec xs ys
   rec x y =
      case getFunction x of
         Just (s, as) ->
            case M.lookup s sm of
               Just f -> f rec as y
               Nothing
                  | isAssociative s -> associativeMatch s rec as y
                  | otherwise       -> defaultMatch rec x y
         _ -> defaultMatch rec x y

defaultMatch :: Match Term -> Match Term
defaultMatch f x y =
   case (x, y) of
      (TCon s xs, TCon t ys) -> do
         guard (s == t)
         matchList f xs ys
      (TList xs, TList ys) ->
         matchList f xs ys
      _ -> do
         guard (x == y)
         return emptySubst

matchList :: Match Term -> Match [Term]
matchList f as bs =
   case safeZipWith f as bs of
      Just ms -> products ms
      Nothing -> fail "matchList: lengths differ"

safeZipWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
safeZipWith f = rec
 where
   rec []     []     = Just []
   rec (a:as) (b:bs) = fmap (f a b:) (rec as bs)
   rec _      _      = Nothing

products :: [[Substitution]] -> [Substitution]
products = foldr op [emptySubst]
 where
   op xs ys = catMaybes [ x @+@ y | x <- xs, y <- ys ]

associativeMatch :: Symbol -> SymbolMatch
associativeMatch s f as b =
   pairingsMatchA make (collects as []) (collect b []) >>= products
 where
   make :: Term -> [Term] -> [Substitution]
   make (TMeta i) xs = [singletonSubst i (construct xs)]
   make x [y]        = f x y
   make _ _          = []

   collects     = foldr ((.) . collect) id
   collect term = maybe (term:) collects (isFunction s term)

   construct xs
      | null xs   = error "associativeMatch: empty list"
      | otherwise = foldr1 (binary s) xs

-----------------------------------------------------------
--- * Test unification properties

unificationTests :: TestSuite
unificationTests = suite "Unification"
   [ useProperty "unify" $ \a b ->
        case unify a b of
           Just s  -> (s |-> a) == (s |-> b)
           Nothing -> True
   , useProperty "unify-succeed" $ \a s ->
        let b = s |-> a in
        case unify a b of
           Just s2 -> (s2 |-> a) == (s2 |-> b)
           Nothing -> False
   , useProperty "match" $ \a b ->
        case match a b of
           Just s  -> (s |-> a) == b
           Nothing -> True
   , useProperty "match-succeed" $ \a s ->
        let b = s |-> a in
        case match a (s |-> a) of
           Just s2 -> (s2 |-> a) == b
           Nothing -> True
   ]