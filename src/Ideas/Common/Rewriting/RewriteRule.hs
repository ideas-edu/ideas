{-# LANGUAGE ExistentialQuantification,
       FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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

module Ideas.Common.Rewriting.RewriteRule
   ( -- * Supporting type class
     Different(..)
     -- * Rewrite rules and specs
   , RewriteRule, ruleSpecTerm, RuleSpec(..)
     -- * Compiling rewrite rules
   , makeRewriteRule, termRewriteRule, RuleBuilder(..)
     -- * Using rewrite rules
   , showRewriteRule
   , metaInRewriteRule, renumberRewriteRule
   , symbolMatcher, symbolBuilder
   ) where

import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rewriting.Substitution
import Ideas.Common.Rewriting.Term
import Ideas.Common.Rewriting.Unification hiding (match)
import Ideas.Common.View
import Ideas.Utils.Uniplate (descend)
import qualified Data.IntSet as IS
import qualified Data.Map as M

------------------------------------------------------
-- Rewrite rules and specs

infixl 1 :~>

data RuleSpec a = a :~> a deriving Show

instance Functor RuleSpec where
   fmap f (a :~> b) = f a :~> f b

data RewriteRule a = R
   { ruleId         :: Id
   , ruleSpecTerm   :: RuleSpec Term
   , ruleShow       :: a -> String
   , ruleTermView   :: View Term a
   , ruleMatchers   :: M.Map Symbol SymbolMatch
   , ruleBuilders   :: M.Map Symbol ([Term] -> Term)
   }

instance Show (RewriteRule a) where
   show = showId

instance HasId (RewriteRule a) where
   getId = ruleId
   changeId f r = r {ruleId = f (ruleId r)}

------------------------------------------------------
-- Compiling a rewrite rule

class Different a where
   different :: (a, a)

instance Different a => Different [a] where
   different = ([], [fst different])

instance Different Int where
   different = (0, 1)

instance Different Integer where
   different = (0, 1)

instance Different Double where
   different = (0, 1)

instance Different Float where
   different = (0, 1)

instance Different Rational where
   different = (0, 1)

instance Different Bool where
   different = (True, False)

instance Different Char where
   different = ('a', 'b')

instance Different Term where
   different = (TNum 0, TNum 1)

instance (Different a, Different b) => Different (a, b) where
   different =
      let (a1, a2) = different
          (b1, b2) = different
      in ((a1, b1), (a2, b2))

instance (Different a, Different b, Different c) => Different (a, b, c) where
   different =
      let (a1, a2) = different
          (b1, b2) = different
          (c1, c2) = different
      in ((a1, b1, c1), (a2, b2, c2))

class (IsTerm a, Show a) => RuleBuilder t a | t -> a where
   buildRuleSpec  :: Int -> t -> RuleSpec Term

instance (IsTerm a, Show a) => RuleBuilder (RuleSpec a) a where
   buildRuleSpec  = const $ fmap toTerm

instance (Different a, RuleBuilder t b) => RuleBuilder (a -> t) b where
   buildRuleSpec i f = buildFunction i (buildRuleSpec (i+1) . f)

buildFunction :: Different a => Int -> (a -> RuleSpec Term) -> RuleSpec Term
buildFunction n f = fzip (fill n) ((f *** f) different)
 where
   fzip g (a :~> b, c :~> d) = g a c :~> g b d

fill :: Int -> Term -> Term -> Term
fill i = rec
 where
   rec (TCon s xs) (TCon t ys) | s == t && length xs == length ys =
      TCon s (zipWith rec xs ys)
   rec (TList xs) (TList ys) | length xs == length ys =
      TList (zipWith rec xs ys)
   rec a b
      | a == b    = a
      | otherwise = TMeta i

buildSpec :: M.Map Symbol SymbolMatch
          -> M.Map Symbol ([Term] -> Term)
          -> RuleSpec Term -> Term -> [(Term, [Term])]
buildSpec sm sb (lhs :~> rhs) a = do
   (sub, ml, mr) <- matchExtended sm lhs a
   let sym = maybe (error "buildSpec") fst (getFunction lhs)
       extLeft  = maybe id (binary sym) ml
       extRight = maybe id (flip (binary sym)) mr
       new  = useBuilders $ extLeft $ extRight $ sub |-> rhs
       args = mapMaybe (`lookupVar` sub) $ IS.toList $ dom sub
   return (new, args)
 where
   useBuilders
      | M.null sb = id
      | otherwise = rec
    where
      rec (TCon s xs) =
         fromMaybe (TCon s) (M.lookup s sb) (map rec xs)
      rec term = term

makeRewriteRule :: (IsId n, RuleBuilder f a) => n -> f -> RewriteRule a
makeRewriteRule s f =
   R (newId s) (buildRuleSpec 0 f) show termView M.empty M.empty

termRewriteRule :: (IsId n, IsTerm a, Show a) => n -> RuleSpec Term -> RewriteRule a
termRewriteRule s spec =
   R (newId s) spec show termView M.empty M.empty

symbolMatcher :: Symbol -> SymbolMatch -> RewriteRule a -> RewriteRule a
symbolMatcher s f r = r {ruleMatchers = M.insert s f (ruleMatchers r)}

symbolBuilder :: Symbol -> ([Term] -> Term) -> RewriteRule a -> RewriteRule a
symbolBuilder s f r = r {ruleBuilders = M.insert s f (ruleBuilders r)}

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where
   applyAll r = map fst . applyRewriteRule r

applyRewriteRule :: RewriteRule a -> a -> [(a, Environment)]
applyRewriteRule r a = do
   let builder = buildSpec (ruleMatchers r) (ruleBuilders r) (ruleSpecTerm r)
       term    = toTermRR r a
   (out, xs) <- builder term
   let env    = mconcat (zipWith make xs [1::Int ..])
       make t = flip singleBinding t . makeRef . show
   b <- maybeToList (fromTermRR r out)
   return (b, env)

-----------------------------------------------------------
-- Pretty-print a rewriteRule

showRewriteRule :: Bool -> RewriteRule a -> Maybe String
showRewriteRule sound r = do
   x <- fromTermRR r (sub |-> a)
   y <- fromTermRR r (sub |-> b)
   let op = if sound then "~>" else "/~>"
   return (ruleShow r x ++ " " ++ op ++ " " ++ ruleShow r y)
 where
   a :~> b = ruleSpecTerm r
   vs  = IS.toList (metaVarSet a `IS.union` metaVarSet b)
   sub = listToSubst $ zip vs [ TVar [c] | c <- ['a' ..] ]

------------------------------------------------------

-- some helpers
metaInRewriteRule :: RewriteRule a -> [Int]
metaInRewriteRule r = metaVars a ++ metaVars b
 where a :~> b = ruleSpecTerm r

renumberRewriteRule :: Int -> RewriteRule a -> RewriteRule a
renumberRewriteRule n r = r {ruleSpecTerm = fmap f (ruleSpecTerm r)}
 where
   f (TMeta i) = TMeta (i+n)
   f term      = descend f term

toTermRR :: RewriteRule a -> a -> Term
toTermRR = build . ruleTermView

fromTermRR :: RewriteRule a -> Term -> Maybe a
fromTermRR = match . ruleTermView