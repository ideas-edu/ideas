module Matrix.Equation where

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
  
getLHS, getRHS :: Equation a -> a
getLHS (x :==: _) = x
getRHS (_ :==: y) = y

evalEquation :: Eq a => Equation a -> Bool
evalEquation = evalEquationWith id

evalEquationWith :: Eq b => (a -> b) -> Equation a -> Bool
evalEquationWith f (x :==: y) = f x == f y

combineWith :: (a -> a -> a) -> Equation a -> Equation a -> Equation a
combineWith f (x1 :==: x2) (y1 :==: y2) = f x1 y1 :==: f x2 y2

-----------------------------------------------------
-- QuickCheck generators

instance Arbitrary a => Arbitrary (Equation a) where
   arbitrary = liftM2 (:==:) arbitrary arbitrary
   coarbitrary (x :==: y) = coarbitrary x . coarbitrary y