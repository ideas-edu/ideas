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
   ( -- * Supporting type classes
     Rewrite(..), Different(..)
     -- * Rewrite rules and specs
   , RewriteRule, ruleSpecTerm, RuleSpec(..)
     -- * Compiling rewrite rules
   , rewriteRule, RuleBuilder
     -- * Using rewrite rules
   , rewrite, rewriteM, showRewriteRule, smartGenerator
   , metaInRewriteRule, renumberRewriteRule, inverseRule
   , useOperators
   ) where

import Common.Classes
import Common.Id
import Common.View hiding (match)
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Rewriting.Group
import Common.Rewriting.Unification hiding (match)
import Common.Uniplate (descend)
import Control.Monad
import Data.Maybe
import Test.QuickCheck
import qualified Common.Rewriting.Unification as Unification
import qualified Data.IntSet as IS
   
------------------------------------------------------
-- Supporting type classes

-- The arbitrary type class is a quick solution to have smart generators
-- (in combination with lifting rules). The function in the RewriteRule module
-- cannot have a type class for this reason
-- The show type class is added for pretty-printing rules
class (IsTerm a, Arbitrary a, Show a) => Rewrite a where
   operators :: [Magma a]
   -- default definition: no special operators
   operators = []

------------------------------------------------------
-- Rewrite rules and specs

infixl 1 :~>
   
data RuleSpec a = a :~> a deriving Show

instance Functor RuleSpec where
   fmap f (a :~> b) = f a :~> f b

data RewriteRule a = R
   { ruleId        :: Id
   , ruleSpecTerm  :: RuleSpec Term
   , ruleOperators :: [Magma a]
   , ruleShow      :: a -> String
   , ruleTermView  :: View Term a
   , ruleGenerator :: Gen a
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

class RuleBuilder t a | t -> a where
   buildRuleSpec :: t -> Int -> RuleSpec Term

instance IsTerm a => RuleBuilder (RuleSpec a) a where
   buildRuleSpec = const . fmap toTerm

instance (Different a, RuleBuilder t b) => RuleBuilder (a -> t) b where
   buildRuleSpec f i = buildFunction i (\a -> buildRuleSpec (f a) (i+1))

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

buildSpec :: [Symbol] -> RuleSpec Term -> Term -> [Term]
buildSpec ops (lhs :~> rhs) a = do
   s <- Unification.match ops lhs a
   let (b1, b2) = (specialLeft `IS.member` dom s, specialRight `IS.member` dom s)
       sym      = maybe (error "buildSpec") fst (getFunction lhs)
       extLeft  x = if b1 then binary sym (Meta specialLeft) x else x
       extRight x = if b2 then binary sym x (Meta specialRight) else x
   return (s |-> extLeft (extRight rhs))

rewriteRule :: (IsId n, RuleBuilder f a, Rewrite a) => n -> f -> RewriteRule a
rewriteRule s f = R (newId s) (buildRuleSpec f 0) operators show termView arbitrary

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where 
   applyAll = rewrite

rewrite :: RewriteRule a -> a -> [a]
rewrite r a = 
   let term = toTermRR r a
       syms = mapMaybe (operatorSymbol r a) (ruleOperators r)
   in concatMap (fromTermRR r) (buildSpec syms (ruleSpecTerm r) term)

operatorSymbol :: IsMagma m => RewriteRule a -> a -> m a -> Maybe Symbol
operatorSymbol r a op = 
   case getFunction (toTermRR r (operation op a a)) of
      Just (s, [_, _]) -> Just s
      _                -> Nothing
 
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

-----------------------------------------------------------
-- Smart generator that creates instantiations of the left-hand side

smartGenerator :: RewriteRule a -> Gen a
smartGenerator r = do 
   let a :~> _ = ruleSpecTerm r
   let vs = IS.toList (metaVarSet a)
   list <- replicateM (length vs) (ruleGenerator r)
   let sub = listToSubst (zip vs (map (toTermRR r) list))
   case fromTermRR r (sub |-> a) of
      Just x  -> return x
      Nothing -> ruleGenerator r

------------------------------------------------------

inverseRule :: RewriteRule a -> RewriteRule a
inverseRule r = r {ruleSpecTerm = b :~> a}
 where a :~> b = ruleSpecTerm r

useOperators :: [Magma a] -> RewriteRule a -> RewriteRule a
useOperators xs r = r {ruleOperators = xs ++ ruleOperators r}

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