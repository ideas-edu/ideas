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
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Equation where

import Common.Unification
import Test.QuickCheck
import Control.Monad

infix 1 :==:

type Equations a = [Equation a]

data Equation  a = a :==: a
   deriving Eq
   
instance Functor Equation where
   fmap f (x :==: y) = f x :==: f y
   
instance Show a => Show (Equation a) where
   show (x :==: y) = show x ++ " == " ++ show y

instance HasVars a => HasVars (Equation a) where
   getVars (x :==: y) = getVars (x, y)
 
getLHS, getRHS :: Equation a -> a
getLHS (x :==: _) = x
getRHS (_ :==: y) = y

evalEquation :: Eq a => Equation a -> Bool
evalEquation = evalEquationWith id

evalEquationWith :: Eq b => (a -> b) -> Equation a -> Bool
evalEquationWith f (x :==: y) = f x == f y

substEquation :: Substitutable a => Substitution a -> Equation a -> Equation a
substEquation sub (x :==: y) = (sub |-> x) :==: (sub |-> y)

substEquations :: Substitutable a => Substitution a -> Equations a -> Equations a
substEquations sub = map (substEquation sub)

combineWith :: (a -> a -> a) -> Equation a -> Equation a -> Equation a
combineWith f (x1 :==: x2) (y1 :==: y2) = f x1 y1 :==: f x2 y2

-----------------------------------------------------
-- QuickCheck generators

instance Arbitrary a => Arbitrary (Equation a) where
   arbitrary = liftM2 (:==:) arbitrary arbitrary
   coarbitrary (x :==: y) = coarbitrary x . coarbitrary y