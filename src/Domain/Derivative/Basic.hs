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

import Common.Context
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
         Diff f     -> ([f],   \[x] -> Diff x)

instance Arbitrary Sym where
   arbitrary = oneof $ map return syms
   coarbitrary a = coarbitrary (elemIndex a syms)
 
instance Arbitrary Expr where
   arbitrary = sized arbFun
   coarbitrary function =
      case function of
         Con r       -> variant 0 . coarbitrary (numerator r) . coarbitrary (denominator r)
         Var s       -> variant 1 . coarbitrary s
         f :+: g     -> variant 2 . coarbitrary f . coarbitrary g
         f :*: g     -> variant 3 . coarbitrary f . coarbitrary g
         f :*: g     -> variant 4 . coarbitrary f . coarbitrary g
         Negate f    -> variant 5 . coarbitrary f
         f :^: g     -> variant 6 . coarbitrary f . coarbitrary g
         f :/: g     -> variant 7 . coarbitrary f . coarbitrary g
         Special s f -> variant 8 . coarbitrary s . coarbitrary f
         Lambda s f  -> variant 9 . coarbitrary s . coarbitrary f
         Diff f      -> variant 10 . coarbitrary f

instance Num Expr where
   (+) = (:+:)
   (*) = (:*:)
   (-) = (:-:)
   negate = Negate
   fromInteger = Con . fromInteger
       
instance Fractional Expr where
   (/) = (:/:)
   fromRational = Con
       
arbFun :: Int -> Gen Expr
arbFun 0 = oneof [ liftM (Con . fromInteger) arbitrary, return (Var "x"), return (Var "y") ]
arbFun n = oneof [ arbFun 0, liftM Diff rec
                 , bin (:+:), bin (:*:), bin (:^:), bin (:/:), liftM2 Special arbitrary rec
                 , liftM (Lambda "x") rec
                 ]
 where
   rec    = arbFun (n `div` 2)
   bin op = liftM2 op rec rec