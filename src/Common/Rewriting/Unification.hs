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
   ( unify, match, matchExtended, matchList
   , Match, SymbolMatch
   , unificationTests
   ) where

import Common.Rewriting.AC (pairingsA)
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Utils.TestSuite
import Control.Arrow
import Control.Monad
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
      (TApp f a, TApp g b) -> 
         rec [f, a] [g, b]
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

match :: MonadPlus m => Term -> Term -> m Substitution
match term1 term2 =
   case (term1, term2) of
      (TMeta i, TMeta j) | i == j ->
         return emptySubst
      (TMeta i, _) | not (i `hasMetaVar` term2) ->
         return (singletonSubst i term2)
      (_, TMeta _) ->
         fail "match: no unifier"
      (TApp f a, TApp g b) ->
         rec [f, a] [g, b]
      (TList xs, TList ys) ->
         rec xs ys
      _ | term1 == term2 ->
         return emptySubst
      _ -> fail "match: no unifier"
 where
   rec [] [] = return emptySubst
   rec (x:xs) (y:ys) = do
      s1 <- match x y
      s2 <- rec (map (s1 |->) xs) ys
      guard (composable s1 s2)
      return (s1 @@ s2)
   rec _ _ = fail "match: no unifier"
   

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
      case getSpine x of
         (TCon s, [_, _]) | isAssociative s -> 
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
defaultMatch f x y = do
   let (a, as) = getSpine x
       (b, bs) = getSpine y
   guard (a == b)
   matchList f as bs

matchList :: Match Term -> Match [Term]
matchList f = rec 
 where
   rec [] [] = return emptySubst
   rec (x:xs) (y:ys) = do
      s1 <- f x y
      s2 <- rec (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   rec _ _ = fail "matchList: lengths differ"

associativeMatch :: Symbol -> SymbolMatch
associativeMatch s f as b =
   concatMap (uncurry (matchList f) . unzip . map make) result
 where
   result = pairingsA True (collects as []) (collect b [])
   make   = construct *** construct

   collects     = foldr ((.) . collect) id
   collect term = maybe (term:) collects (isFunction s term)

   construct xs
      | null xs   = error "associativeMatch: empty list"
      | otherwise = foldr1 (binary s) xs

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