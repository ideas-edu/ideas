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
-- Mathematical inequalities
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Inequality where

import Common.Traversable
import Data.Maybe
import Test.QuickCheck
import Control.Monad

-----------------------------------------------------------------------------
-- Inequality data type

infix 1 :<:, :>:, :<=:, :>=:, :/=:

data Inequality a = a :<:  a | a :>:  a
                  | a :<=: a | a :>=: a
                  | a :/=: a
   deriving (Eq, Ord)

-----------------------------------------------------------------------------
-- Helper data type for operators, and two convenience conversion functions

data Operator = LessThan | GreaterThan 
              | LessThanOrEqualTo | GreaterThanOrEqualTo
              | NotEqualTo
   deriving (Show, Eq, Ord, Enum)
              
toOperator :: Inequality a -> (a, Operator, a)
toOperator ineq = 
   case ineq of
      x :<:  y -> (x, LessThan, y)
      x :>:  y -> (x, GreaterThan, y)
      x :<=: y -> (x, LessThanOrEqualTo, y)
      x :>=: y -> (x, GreaterThanOrEqualTo, y)
      x :/=: y -> (x, NotEqualTo, y)

fromOperator :: (a, Operator, a) -> Inequality a
fromOperator (x, op, y) = 
   case op of
      LessThan             -> x :<: y
      GreaterThan          -> x :>: y
      LessThanOrEqualTo    -> x :<=: y
      GreaterThanOrEqualTo -> x :>=: y
      NotEqualTo           -> x :/=: y

-----------------------------------------------------------------------------
-- Instance declarations

instance Functor Inequality where
   fmap f ineq = 
      let (x, op, y) = toOperator ineq
      in fromOperator (f x, op, f y)

instance Once Inequality where
   onceM f ineq =
      let (x, op, y) = toOperator ineq
      in liftM (\a -> fromOperator (a, op, y)) (f x) `mplus`
         liftM (\a -> fromOperator (x, op, a)) (f y)

instance Switch Inequality where
   switch ineq =
      let (mx, op, my) = toOperator ineq 
      in liftM2 (\x y -> fromOperator (x, op, y)) mx my

instance Crush Inequality where
   crush ineq =
      let (x, _, y) = toOperator ineq
      in [x, y]

instance Show a => Show (Inequality a) where
   show ineq =
      let (x, op, y) = toOperator ineq
          f = fromMaybe "??" . (`lookup` list)
          list = [ (LessThan, "<"), (GreaterThan, ">"), (NotEqualTo, "/=")
                 , (LessThanOrEqualTo, "<="), (GreaterThanOrEqualTo, ">=")
                 ]
      in unwords [show x, f op, show y]
 
-----------------------------------------------------------------------------
-- Utility functions

getLHS :: Inequality a -> a
getLHS ineq = x 
 where (x, _, _) = toOperator ineq
 
getRHS :: Inequality a -> a 
getRHS ineq = y
 where (_, _, y) = toOperator ineq

flipSides :: Inequality a -> Inequality a
flipSides ineq = 
   let (x, op, y) = toOperator ineq
       f = fromMaybe op . (`lookup` list)
       list = [ (LessThan, GreaterThan), (GreaterThan, LessThan)
              , (NotEqualTo, NotEqualTo)
              , (LessThanOrEqualTo, GreaterThanOrEqualTo)
              , (GreaterThanOrEqualTo, LessThanOrEqualTo)
              ]
   in fromOperator (x, f op, y)

-----------------------------------------------------------------------------
-- QuickCheck generators

instance Arbitrary a => Arbitrary (Inequality a) where
   arbitrary = 
      let f x op y = fromOperator (x, op, y)
      in liftM3 f arbitrary arbitrary arbitrary
   coarbitrary = coarbitrary . toOperator
   
instance Arbitrary Operator where
   arbitrary = oneof $ map return [LessThan .. NotEqualTo]
   coarbitrary op = variant (fromEnum op)