{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, 
      FunctionalDependencies, FlexibleInstances, UndecidableInstances,
      TypeSynonymInstances #-}
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
   , RewriteRule, rulePair, RuleSpec(..)
     -- * Compiling a rewrite rule
   , rewriteRule, rewriteRules, Builder, BuilderList
     -- * Using rewrite rules
   , rewrite, rewriteM, showRewriteRule, smartGenerator
   , metaInRewriteRule, renumberRewriteRule
   ) where

import Common.Classes
import Common.Id
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Rewriting.Unification
import Common.Uniplate (descend, leafs)
import Control.Monad
import Data.List
import Test.QuickCheck

------------------------------------------------------
-- Supporting type classes

-- The arbitrary type class is a quick solution to have smart generators
-- (in combination with lifting rules). The function in the RewriteRule module
-- cannot have a type class for this reason
-- The show type class is added for pretty-printing rules
class (IsTerm a, Arbitrary a, Show a) => Rewrite a where
   operators :: [(Operator a, Symbol)]
   -- default definition: no special operators
   operators = []

------------------------------------------------------
-- Rewrite rules and specs

infixl 1 :~>
   
data RuleSpec a = a :~> a deriving Show

data RewriteRule a = Rewrite a => R 
   { ruleId   :: Id
   , rulePair :: RuleSpec Term
   }
   
instance Show (RewriteRule a) where
   show = showId

instance Functor RuleSpec where
   fmap f (a :~> b) = f a :~> f b

instance HasId (RewriteRule a) where
   getId        = ruleId
   changeId f r@(R _ _) = R {ruleId = f (ruleId r), rulePair = rulePair r}

-- constructor
newRule :: Rewrite a => String -> (Int -> RuleSpec Term) -> RewriteRule a
newRule s f = R (newId s) (f 0)

------------------------------------------------------
-- Compiling a rewrite rule

class Different a where
   different :: (a, a)

class Builder t a | t -> a where
   buildSpec :: t -> Int -> RuleSpec Term

instance IsTerm a => Builder (RuleSpec a) a where
   buildSpec (a :~> b) _ = toTerm a :~> toTerm b

instance (Different a, Builder t b) => Builder (a -> t) b where
   buildSpec f i = buildFunction i (\a -> buildSpec (f a) (i+1))

class BuilderList t a | t -> a where
   getSpecNr  :: t -> Int -> Int -> RuleSpec Term
   countSpecs :: t -> Int
 
instance Builder t a => BuilderList [t] a where
   getSpecNr rs = buildSpec . (rs !!)
   countSpecs   = length

instance (Different a, BuilderList t b) => BuilderList (a -> t) b where 
   getSpecNr f n i = buildFunction i (\a -> getSpecNr (f a) n (i+1))
   countSpecs  f   = countSpecs (f $ error "countSpecsL")

buildFunction :: Different a => Int -> (a -> RuleSpec Term) -> RuleSpec Term
buildFunction n f = fill n a1 a2 :~> fill n b1 b2
 where
   a1 :~> b1 = f (fst different)
   a2 :~> b2 = f (snd different)

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

rewriteRule :: (Builder f a, Rewrite a) => String -> f -> RewriteRule a
rewriteRule s = newRule s . buildSpec

rewriteRules :: (BuilderList f a, Rewrite a) => String -> f -> [RewriteRule a]
rewriteRules s f = 
   [ newRule s (getSpecNr f n) | n <- [0 .. countSpecs f-1] ]

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where 
   applyAll = rewrite

rewrite :: RewriteRule a -> a -> [a]
rewrite r@(R _ _) a = build (getOps a operators) (rulePair r) a
 where
   getOps :: a -> [(Operator a, Symbol)] -> [Symbol]
   getOps = const (map snd)
 
rewriteM :: MonadPlus m => RewriteRule a -> a -> m a
rewriteM r = msum . map return . rewrite r

-----------------------------------------------------------
-- Pretty-print a rewriteRule

showRewriteRule :: Bool -> RewriteRule a -> Maybe String
showRewriteRule sound r@(R _ _) = do
   x <- fromTermTp r (sub |-> a)
   y <- fromTermTp r (sub |-> b)
   let op = if sound then "~>" else "/~>" 
   return (show x ++ " " ++ op ++ " " ++ show y)
 where
   a :~> b = rulePair r
   vs  = (getMetaVars a `union` getMetaVars b)
   sub = listToSubst $ zip vs [ Var [c] | c <- ['a' ..] ]
   
   fromTermTp :: IsTerm a => RewriteRule a -> Term -> Maybe a
   fromTermTp _ = fromTerm

-----------------------------------------------------------
-- Smart generator that creates instantiations of the left-hand side

smartGenerator :: RewriteRule a -> Gen a
smartGenerator r@(R _ _) = do 
   let a :~> _ = rulePair r
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

-- some helpers
metaInRewriteRule :: RewriteRule a -> [Int]
metaInRewriteRule r =
   let lhs :~> rhs = rulePair r
   in [ n | Meta n <- leafs lhs ++ leafs rhs ]

renumberRewriteRule :: Int -> RewriteRule a -> RewriteRule a
renumberRewriteRule n r@(R _ _) = R {ruleId = ruleId r, rulePair = f lhs :~> f rhs}
 where
   lhs :~> rhs = rulePair r
   f (Meta i) = Meta (i+n)
   f term     = descend f term