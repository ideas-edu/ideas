{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, 
      FunctionalDependencies, FlexibleInstances, UndecidableInstances,
      TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
     Rewrite(..), ShallowEq(..), Different(..)
     -- * Rewrite rules and specs
   , RewriteRule(ruleName, rulePair), RuleSpec(..)
     -- * Compiling a rewrite rule
   , rewriteRule, rewriteRules, Builder, BuilderList
     -- * Using rewrite rules
   , rewrite, rewriteM, smartGenerator
   ) where

import Common.Uniplate hiding (rewrite, rewriteM)
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Control.Monad
import Data.List
import Data.Maybe
import Test.QuickCheck
import Common.Apply
import Common.Rewriting.MetaVar (getMetaVars)
import Common.Rewriting.Unification
import qualified Data.IntSet as IS

------------------------------------------------------
-- Supporting type classes

class Different a where
   different :: (a, a)

class ShallowEq a where 
   shallowEq :: a -> a -> Bool

-- The arbitrary type class is a quick solution to have smart generators
-- (in combination with lifting rules). The function in the RewriteRule module
-- cannot have a type class for this reason
-- The show type class is added for pretty-printing rules
class (IsTerm a, Arbitrary a, Different a, ShallowEq a, Uniplate a) => Rewrite a where
   operators :: [Operator a]
   -- default definition: no associative/commutative operators
   operators = []

------------------------------------------------------
-- Rewrite rules and specs

infixl 1 :~>
   
data RuleSpec a = a :~> a deriving Show

data RewriteRule a = Rewrite a => R 
   { ruleName     :: String
   , nrOfMetaVars :: Int
   , rulePair     :: Int -> RuleSpec Term 
   }

------------------------------------------------------
-- Compiling a rewrite rule

class Builder t a | t -> a where
   buildSpec :: t -> Int -> RuleSpec Term
   countVars :: t -> Int

instance IsTerm a => Builder (RewriteRule a) a where
   buildSpec = rulePair
   countVars = nrOfMetaVars

instance IsTerm a => Builder (RuleSpec a) a where
   buildSpec (a :~> b) _ = toTerm a :~> toTerm b
   countVars _    = 0

instance (Different a, Builder t b) => Builder (a -> t) b where
   buildSpec f i = buildFunction i (\a -> buildSpec (f a) (i+1))
   countVars f   = countVars (f $ error "countVars") + 1

class BuilderList t a | t -> a where
   getSpecNr   :: t -> Int -> Int -> RuleSpec Term
   countSpecsL :: t -> Int
   countVarsL  :: t -> Int

instance Rewrite a => BuilderList (RewriteRule a) a where
   getSpecNr r n = if n==0 then rulePair r else error "getSpecNr"
   countSpecsL _ = 1
   countVarsL    = nrOfMetaVars
 
instance Builder t a => BuilderList [t] a where
   getSpecNr rs = buildSpec . (rs !!)
   countSpecsL  = length
   countVarsL _ = 0

instance (Different a, BuilderList t b) => BuilderList (a -> t) b where 
   getSpecNr f n i = buildFunction i (\a -> getSpecNr (f a) n (i+1))
   countSpecsL f   = countSpecsL (f $ error "countSpecsL")
   countVarsL f    = countVarsL (f $ error "countSpecsL") + 1

buildFunction :: Different a => Int -> (a -> RuleSpec Term) -> RuleSpec Term
buildFunction n f = fill n a1 a2 :~> fill n b1 b2
 where
   a1 :~> b1 = f (fst different)
   a2 :~> b2 = f (snd different)

fill :: Int -> Term -> Term -> Term
fill i (App a1 a2) (App b1 b2) = App (fill i a1 b1) (fill i a2 b2)
fill i a b 
   | a == b    = a
   | otherwise = Meta i
   
buildTerm :: Operators Term -> RuleSpec Term -> Term -> [Term]
buildTerm ops (lhs :~> rhs) a = do
   s <- match ops lhs a
   return (s |-> rhs)
   
build :: Rewrite a => RuleSpec Term -> a -> [a]
build p a = buildTerm (map convOp (getOp a)) p (toTerm a) >>= (maybe [] return . fromTerm)
 where
   getOp :: Rewrite a => a -> Operators a
   getOp _ = operators

rewriteRule :: (Builder f a, Rewrite a) => String -> f -> RewriteRule a
rewriteRule s f = R s (countVars f) (buildSpec f)

rewriteRules :: (BuilderList f a, Rewrite a) => String -> f -> [RewriteRule a]
rewriteRules s f = map (R s (countVarsL f) . getSpecNr f) [0 .. countSpecsL f-1]

------------------------------------------------------
-- Using a rewrite rule

instance Apply RewriteRule where 
   applyAll = rewrite

rewrite :: RewriteRule a -> a -> [a]
rewrite r@(R _ _ _) a = do
   ext <- extendContext operators r
   build (rulePair ext 0) a

rewriteM :: MonadPlus m => RewriteRule a -> a -> m a
rewriteM r = msum . map return . rewrite r

-----------------------------------------------------------
-- Smart generator that creates instantiations of the left-hand side

smartGenerator :: RewriteRule a -> Gen a
smartGenerator r@(R _ _ _) = do 
   let a :~> _ = rulePair r 0
   let vs      = IS.toList (getMetaVars a)
   list <- vector (length vs) 
   let sub = listToSubst (zip vs (map (tpToTerm r) list))
   case fromTerm (sub |-> a) of
      Just a  -> return a
      Nothing -> arbitrary
 where
   tpToTerm :: IsTerm a => RewriteRule a -> a -> Term
   tpToTerm _ = toTerm

------------------------------------------------------

-- Bug fix 4/3/2009: for associative operators, we need to extend rewrite
-- rules to take "contexts" into account. In addition to a left and a right
-- context, we also should consider a context on both sides. If not, we 
-- might miss some locations, as pointed out by Josje's bug report.
extendContext :: Operators a -> RewriteRule a -> [RewriteRule a]
extendContext ops r@(R _ _ _) =
   case findOp2 ops (lhs $ rulePair r 0) of
      Just op | isAssociative op -> 
         [r, extend (leftContext op) r, extend (rightContext op) r 
         , extend (rightContext op) (extend (leftContext op) r) ]
      _ -> [r]
 where
   lhs (a :~> _) = a
 
   leftContext op a (x :~> y) =
      constructor op a x :~> constructor op a y
   
   rightContext op a (x :~> y) =
      constructor op x a :~> constructor op y a

extend :: (Term -> RuleSpec Term -> RuleSpec Term) -> RewriteRule a -> RewriteRule a
extend f (R s n g) = R s (n+1) (\i -> f (Meta (i+n)) (g i))
      
findOp2 :: (IsTerm a, Different a) => Operators a -> Term -> Maybe (Operator Term)
findOp2 [] _ = Nothing
findOp2 (o:os) a = 
   case findOperator [convOp o] a of
      Just _  -> Just (convOp o)
      Nothing -> findOp2 os a

convOp :: (IsTerm a, Different a) => Operator a -> Operator Term
convOp op = op 
   { constructor = binary s
   , destructor  = isBinary s
   }
 where
   s = let a = fst different
       in case getSpine (toTerm (constructor op a a)) of
             (Con s, [_,_]) -> s
             _ -> ""

