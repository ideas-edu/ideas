-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Derivative.Basic where

import Common.Uniplate
import Control.Monad
import Data.List
import Data.Ratio
import Test.QuickCheck

data Expr
   = Con Rational 
   | Var String
   | Expr :+: Expr 
   | Expr :*: Expr 
   | Expr :-: Expr
   | Negate Expr
   | Expr :^: Expr 
   | Expr :/: Expr 
   | Special Sym Expr
   | Lambda String Expr
   | Diff Expr
   | MetaVar String
 deriving (Show, Read, Eq)
 
data Sym = Sin | Cos | Ln
   deriving (Show, Read, Eq, Enum)
 
syms :: [Sym]
syms = [Sin .. Ln]
 
noDiff :: Expr -> Bool
noDiff f = null [ () | Diff _ <- universe f ]
  
instance Uniplate Expr where
   uniplate function =
      case function of
         Con r          -> ([], \_ -> Con r)
         Var s          -> ([], \_ -> Var s)
         f :+: g        -> ([f,g], \[x,y] -> x :+: y)
         f :*: g        -> ([f,g], \[x,y] -> x :*: y)
         f :-: g        -> ([f,g], \[x,y] -> x :-: y)
         Negate f       -> ([f], \[x] -> Negate x)
         f :^: g        -> ([f,g], \[x,y] -> x :^: y)
         f :/: g        -> ([f,g], \[x,y] -> x :/: y)
         Special s f    -> ([f], \[x] -> Special s x)
         Lambda s f     -> ([f], \[x] -> Lambda s x)
         Diff f         -> ([f],   \[x] -> Diff x)
         MetaVar s      -> ([], \_ -> MetaVar s)

instance Arbitrary Sym where
   arbitrary = oneof $ map return syms
   coarbitrary a = coarbitrary (elemIndex a syms)
 
instance Arbitrary Expr where
   arbitrary = sized arbExpr
   coarbitrary function =
      case function of
         Con r       -> variant 0 . coarbitrary (numerator r) . coarbitrary (denominator r)
         Var s       -> variant 1 . coarbitrary s
         f :+: g     -> variant 2 . coarbitrary f . coarbitrary g
         f :*: g     -> variant 3 . coarbitrary f . coarbitrary g
         Negate f    -> variant 4 . coarbitrary f
         f :^: g     -> variant 5 . coarbitrary f . coarbitrary g
         f :/: g     -> variant 6 . coarbitrary f . coarbitrary g
         Special s f -> variant 7 . coarbitrary s . coarbitrary f
         Lambda s f  -> variant 8 . coarbitrary s . coarbitrary f
         Diff f      -> variant 9 . coarbitrary f
         MetaVar s   -> variant 10 . coarbitrary s

arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [ liftM (Con . fromInteger) arbitrary, return (Var "x"), return (Var "y") ]
arbExpr n = oneof [ arbExpr 0, liftM Diff rec
                 , bin (:+:), bin (:*:), bin (:^:), bin (:/:), liftM2 Special arbitrary rec
                 , liftM (Lambda "x") rec
                 ]
 where
   rec    = arbExpr (n `div` 2)
   bin op = liftM2 op rec rec
   
instance Num Expr where
   (+) = (:+:)
   (*) = (:*:)
   (-) = (:-:)
   negate = Negate
   fromInteger = Con . fromInteger
   abs = error "abs not defined"
   signum = error "signum not defined"
       
instance Fractional Expr where
   (/) = (:/:)
   fromRational = Con

{-
instance HasVars Expr where
   getVarsList e = [ x | MetaVar x <- universe e ]

instance MakeVar Expr where
   makeVar = MetaVar
   
instance Substitutable Expr where 
   sub |-> e@(MetaVar x) = fromMaybe e (lookupVar x sub)
   sub |-> e = let (as, f) = uniplate e 
               in f (map (sub |->) as)
       
instance Unifiable Expr where
   unify = unifyExpr
   
unifyExpr :: Expr -> Expr -> Maybe (Substitution Expr)
unifyExpr e1 e2 = 
   case (e1, e2) of
      (MetaVar x, MetaVar y) | x==y      -> return emptySubst
      (MetaVar x, _) | not (x `S.member` getVars e2) -> return (singletonSubst x e2)
      (_, MetaVar y) | not (y `S.member` getVars e1) -> return (singletonSubst y e1)
      (Var x, Var y) -> if x==y then return emptySubst else Nothing
      (Con x, Con y) -> if x==y then return emptySubst else Nothing
      (Special f _, Special g _) | f /= g -> Nothing
      (Lambda x e1, Lambda y e2) | x /= y -> unifyExpr (renameVar x y e1) e2
      _ -> if (exprToConNr e1 == exprToConNr e2) 
           then unifyList (children e1) (children e2)
           else Nothing

renameVar :: String -> String -> Expr -> Expr
renameVar x y = rec 
 where
   rec (Var z) 
      | z==x = Var y
   rec (Lambda z e) 
      | z==x || z==y = rec (Lambda (z++"_") (renameVar z (z++"_") e))
   rec e = let (es, f) = uniplate e
           in f (map rec es)

exprToConNr :: Expr -> Int
exprToConNr expr =
   case expr of
      Var _       -> 0
      Con _       -> 1
      _ :+: _     -> 2
      _ :*: _     -> 3
      _ :-: _     -> 4
      _ :^: _     -> 5
      _ :/: _     -> 6
      Lambda _ _  -> 7
      Negate _    -> 8
      Diff  _     -> 9
      Special _ _ -> 10
      MetaVar _   -> (-1) -}