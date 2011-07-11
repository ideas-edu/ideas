{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, 
       FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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
module Common.Rewriting.RewriteRule 
   ( -- * Supporting type class
     Different(..)
     -- * Rewrite rules and specs
   , RewriteRule, ruleSpecTerm, RuleSpec(..)
     -- * Compiling rewrite rules
   , rewriteRule, RuleBuilder(..)
     -- * Using rewrite rules
   , rewrite, rewriteM, showRewriteRule, smartGenerator
   , metaInRewriteRule, renumberRewriteRule
   ) where

import Common.Classes
import Common.Id
import Common.View hiding (match)
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Rewriting.Unification
import Common.Uniplate (descend)
import Control.Monad
import Test.QuickCheck
import qualified Data.IntSet as IS

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
   rec (Apply f a) (Apply g b) = Apply (rec f g) (rec a b)
   rec a b 
      | a == b    = a
      | otherwise = Meta i

buildSpec :: RuleSpec Term -> Term -> [Term]
buildSpec (lhs :~> rhs) a = do
   s <- matchA lhs a
   let (b1, b2) = (specialLeft `IS.member` dom s, specialRight `IS.member` dom s)
       sym      = maybe (error "buildSpec") fst (getFunction lhs)
       extLeft  x = if b1 then binary sym (Meta specialLeft) x else x
       extRight x = if b2 then binary sym x (Meta specialRight) else x
   return (s |-> extLeft (extRight rhs))

rewriteRule :: (IsId n, RuleBuilder f a) => n -> f -> RewriteRule a
rewriteRule s f = R (newId s) (buildRuleSpec 0 f) show termView (buildGenerator f)

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where 
   applyAll = rewrite

rewrite :: RewriteRule a -> a -> [a]
rewrite r a = 
   concatMap (fromTermRR r) $ buildSpec (ruleSpecTerm r) $ toTermRR r a
 
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
   sub = listToSubst $ zip vs [ Var [c] | c <- ['a' ..] ]

------------------------------------------------------

-- some helpers
metaInRewriteRule :: RewriteRule a -> [Int]
metaInRewriteRule r = metaVars a ++ metaVars b
 where a :~> b = ruleSpecTerm r

renumberRewriteRule :: Int -> RewriteRule a -> RewriteRule a
renumberRewriteRule n r = r {ruleSpecTerm = fmap f (ruleSpecTerm r)}
 where
   f (Meta i) = Meta (i+n)
   f term     = descend f term
   
toTermRR :: RewriteRule a -> a -> Term
toTermRR = build . ruleTermView

fromTermRR :: Monad m => RewriteRule a -> Term -> m a
fromTermRR = matchM . ruleTermView