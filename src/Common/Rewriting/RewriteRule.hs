{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
       FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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
module Common.Rewriting.RewriteRule
   ( -- * Supporting type class
     Different(..)
     -- * Rewrite rules and specs
   , RewriteRule, ruleSpecTerm, RuleSpec(..)
     -- * Compiling rewrite rules
   , rewriteRule, RuleBuilder(..)
     -- * Using rewrite rules
   , rewrite, rewriteM, rewriteArgs, showRewriteRule, smartGenerator
   , metaInRewriteRule, renumberRewriteRule
   , symbolMatcher, symbolBuilder
   ) where

import Common.Classes
import Common.Id
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Rewriting.Unification
import Common.Utils.Uniplate (descend)
import Common.View hiding (match)
import Control.Monad
import Data.Maybe
import Test.QuickCheck
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
   , smartGenerator :: Gen a
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

instance Different Char where
   different = ('a', 'b')

class (IsTerm a, Show a) => RuleBuilder t a | t -> a where
   buildRuleSpec  :: Int -> t -> RuleSpec Term
   buildGenerator :: t -> Gen a

instance (IsTerm a, Show a) => RuleBuilder (RuleSpec a) a where
   buildRuleSpec  = const $ fmap toTerm
   buildGenerator (a :~> _) = return a

instance (Arbitrary a, Different a, RuleBuilder t b) => RuleBuilder (a -> t) b where
   buildRuleSpec i f = buildFunction i (buildRuleSpec (i+1) . f)
   buildGenerator f  = liftM f arbitrary >>= buildGenerator

buildFunction :: Different a => Int -> (a -> RuleSpec Term) -> RuleSpec Term
buildFunction n f = fzip (fill n) ((f *** f) different)
 where
   fzip g (a :~> b, c :~> d) = g a c :~> g b d

fill :: Int -> Term -> Term -> Term
fill i = rec
 where
   rec (TApp f a) (TApp g b) = TApp (rec f g) (rec a b)
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
       args = catMaybes $ map (`lookupVar` sub) $ IS.toList $ dom sub
   return (new, args)
 where
   useBuilders
      | M.null sb = id
      | otherwise = rec
    where
      rec term = 
         let (b, bs) = mapSecond (map rec) (getSpine term)
             make = fromMaybe (makeTerm b) (getSymbol b >>= (`M.lookup` sb))
         in make bs

rewriteRule :: (IsId n, RuleBuilder f a) => n -> f -> RewriteRule a
rewriteRule s f = 
   R (newId s) (buildRuleSpec 0 f) show termView M.empty M.empty (buildGenerator f)

symbolMatcher :: Symbol -> SymbolMatch -> RewriteRule a -> RewriteRule a
symbolMatcher s f r = r {ruleMatchers = M.insert s f (ruleMatchers r)}

symbolBuilder :: Symbol -> ([Term] -> Term) -> RewriteRule a -> RewriteRule a
symbolBuilder s f r = r {ruleBuilders = M.insert s f (ruleBuilders r)}

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where
   applyAll = rewrite

rewriteArgs :: RewriteRule a -> a -> [(a, [Term])]
rewriteArgs r a = do
   let term = toTermRR r a
   (out, args) <- buildSpec (ruleMatchers r) (ruleBuilders r) (ruleSpecTerm r) term
   b   <- fromTermRR r out 
   return (b, args)

rewrite :: RewriteRule a -> a -> [a]
rewrite r = map fst . rewriteArgs r

rewriteM :: MonadPlus m => RewriteRule a -> a -> m a
rewriteM r = msum . map return . rewrite r

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

fromTermRR :: Monad m => RewriteRule a -> Term -> m a
fromTermRR = matchM . ruleTermView