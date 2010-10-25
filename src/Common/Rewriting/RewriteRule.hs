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
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Rewriting.Group
import Common.Rewriting.Unification
import Common.Uniplate (descend, leafs)
import Control.Monad
import Data.List
import Data.Maybe
import Test.QuickCheck
   
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

instance Crush RuleSpec where
   crush (a :~> b) = [a, b]

instance Zip RuleSpec where 
   fzipWith f (a :~> b) (c :~> d) = f a c :~> f b d

data RewriteRule a = (Arbitrary a, IsTerm a, Show a) => R 
   { ruleId        :: Id
   , ruleSpecTerm  :: RuleSpec Term
   , ruleOperators :: [Magma a]
   }
   
instance Show (RewriteRule a) where
   show = showId

instance HasId (RewriteRule a) where
   getId = ruleId
   changeId f (R n p ops) = R (f n) p ops

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

buildFunction :: (Zip f, Different a) => Int -> (a -> f Term) -> f Term
buildFunction n f = fzipWith (fill n) (f a) (f b)
 where (a, b) = different
 
fill :: Int -> Term -> Term -> Term
fill i = rec
 where
   rec (Apply f a) (Apply g b) = Apply (rec f g) (rec a b)
   rec a b 
      | a == b    = a
      | otherwise = Meta i

build :: IsTerm a => [Symbol] -> RuleSpec Term -> a -> [a]
build ops (lhs :~> rhs) a = do
   s <- match ops lhs (toTerm a)
   let (b1, b2) = (specialLeft `elem` dom s, specialRight `elem` dom s)
       sym      = maybe (error "build") fst (getConSpine lhs)
       extLeft  a = if b1 then binary sym (Meta specialLeft) a else a
       extRight a = if b2 then binary sym a (Meta specialRight) else a
   fromTermM (s |-> extLeft (extRight rhs))

rewriteRule :: (IsId n, RuleBuilder f a, Rewrite a) => n -> f -> RewriteRule a
rewriteRule s f = R (newId s) (buildRuleSpec f 0) operators

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where 
   applyAll = rewrite

rewrite :: RewriteRule a -> a -> [a]
rewrite r@(R _ _ _) a = 
   build (mapMaybe (operatorSymbol a) (ruleOperators r)) (ruleSpecTerm r) a

operatorSymbol :: (IsMagma m, IsTerm a) => a -> m a -> Maybe Symbol
operatorSymbol a op = 
   case getConSpine (toTerm (operation op a a)) of
      Just (s, [_, _]) -> Just s
      _                -> Nothing
 
rewriteM :: MonadPlus m => RewriteRule a -> a -> m a
rewriteM r = msum . map return . rewrite r

-----------------------------------------------------------
-- Pretty-print a rewriteRule

showRewriteRule :: Bool -> RewriteRule a -> Maybe String
showRewriteRule sound r@(R _ _ _) = do
   x <- fromTermTp r (sub |-> a)
   y <- fromTermTp r (sub |-> b)
   let op = if sound then "~>" else "/~>" 
   return (show x ++ " " ++ op ++ " " ++ show y)
 where
   a :~> b = ruleSpecTerm r
   vs  = (getMetaVars a `union` getMetaVars b)
   sub = listToSubst $ zip vs [ Var [c] | c <- ['a' ..] ]
   
   fromTermTp :: IsTerm a => RewriteRule a -> Term -> Maybe a
   fromTermTp _ = fromTerm

-----------------------------------------------------------
-- Smart generator that creates instantiations of the left-hand side

smartGenerator :: RewriteRule a -> Gen a
smartGenerator r@(R _ _ _) = do 
   let a :~> _ = ruleSpecTerm r
   let vs      = getMetaVars a
   list <- vector (length vs)
   let sub = listToSubst (zip vs (map (tpToTerm r) list))
   case fromTerm (sub |-> a) of
      Just a  -> return a
      Nothing -> arbitrary
 where
   tpToTerm :: IsTerm a => RewriteRule a -> a -> Term
   tpToTerm _ = toTerm

------------------------------------------------------

inverseRule :: RewriteRule a -> RewriteRule a
inverseRule (R n (a :~> b) ops) = R n (b :~> a) ops

useOperators :: [Magma a] -> RewriteRule a -> RewriteRule a
useOperators xs (R n p ops) = R n p (xs ++ ops)

-- some helpers
metaInRewriteRule :: RewriteRule a -> [Int]
metaInRewriteRule r =
   [ n | a <- crush (ruleSpecTerm r), Meta n <- leafs a ]

renumberRewriteRule :: Int -> RewriteRule a -> RewriteRule a
renumberRewriteRule n (R s p ops) = R s (fmap f p) ops
 where
   f (Meta i) = Meta (i+n)
   f term     = descend f term