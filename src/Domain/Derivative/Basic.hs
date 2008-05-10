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
import Data.Ratio
import Test.QuickCheck

data Function 
   = Con Rational 
   | Var String
   | Function :+: Function 
   | Function :*: Function 
   | Function :^: Function 
   | Function :/: Function 
   | Function :.: Function 
   | Derivative String Function
 deriving (Show, Read, Eq)
 
noDerivative :: Function -> Bool
noDerivative f = null [ () | Derivative t _ <- universe f ]
  
instance Uniplate Function where
   uniplate function =
      case function of
         Con r          -> ([], \_ -> Con r)
         Var s          -> ([], \_ -> Var s)
         f :+: g        -> ([f,g], \[x,y] -> x :+: y)
         f :*: g        -> ([f,g], \[x,y] -> x :*: y)
         f :^: g        -> ([f,g], \[x,y] -> x :^: y)
         f :/: g        -> ([f,g], \[x,y] -> x :/: y)
         f :.: g        -> ([f,g], \[x,y] -> x :.: y)
         Derivative s f -> ([f],   \[x] -> Derivative s x)
  
instance Arbitrary Function where
   arbitrary = sized arbFun
   coarbitrary function =
      case function of
         Con r          -> variant 0 . coarbitrary (numerator r) . coarbitrary (denominator r)
         Var s          -> variant 1 . coarbitrary s
         f :+: g        -> variant 2 . coarbitrary f . coarbitrary g
         f :*: g        -> variant 3 . coarbitrary f . coarbitrary g
         f :^: g        -> variant 4 . coarbitrary f . coarbitrary g
         f :/: g        -> variant 5 . coarbitrary f . coarbitrary g
         f :.: g        -> variant 6 . coarbitrary f . coarbitrary g
         Derivative s f -> variant 7 . coarbitrary s . coarbitrary f
         
arbFun :: Int -> Gen Function
arbFun 0 = oneof [ liftM (Con . fromInteger) arbitrary, return (Var "x"), return (Var "y") ]
arbFun n = oneof [ arbFun 0, liftM (Derivative "x") rec, liftM (Derivative "y") rec
                 , bin (:+:), bin (:*:), bin (:^:), bin (:/:), bin (:.:)
                 ]
 where
   rec    = arbFun (n `div` 2)
   bin op = liftM2 op rec rec