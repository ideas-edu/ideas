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
-- Mathematical relations
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Relation 
   ( Relational(..), mapLeft, mapRight, updateLeft, updateRight
   , Relation, relationType, RelationType(..)
   , makeType, (.==.), (./=.), (.<.), (.>.), (.<=.), (.>=.), (.~=.)
   , Equations, Equation(..), equationView
   , Inequality(..)
   ) where

import Common.View
import Common.Traversable
import Data.Maybe
import Test.QuickCheck
import Control.Monad

-----------------------------------------------------------------------------
-- Type class for relations

class Functor f => Relational f where
   leftHandSide  :: f a -> a
   rightHandSide :: f a -> a
   flipSides     :: f a -> f a
   constructor   :: f a -> (b -> b -> f b)

mapLeft, mapRight :: Relational f => (a -> a) -> f a -> f a
mapLeft  f p = updateLeft  (f (leftHandSide p))  p
mapRight f p = updateRight (f (rightHandSide p)) p

updateLeft, updateRight :: Relational f => a -> f a -> f a
updateLeft  a p = constructor p a (rightHandSide p)
updateRight a p = constructor p (leftHandSide p) a

-----------------------------------------------------------------------------
-- Relation data type

data Relation a = R { lhs :: a, relationType :: RelationType, rhs :: a }
   deriving (Eq, Ord)

-- Corresponds exactly to the symbols in the relation1 OpenMath dictionary
data RelationType = EqualTo | NotEqualTo | LessThan | GreaterThan 
                  | LessThanOrEqualTo | GreaterThanOrEqualTo | Approximately
   deriving (Show, Eq, Ord, Enum)

instance Show a => Show (Relation a) where
   show r = unwords [show (lhs r), showRelType (relationType r), show (rhs r)]

instance Functor Relation where
   fmap f (R x rt y) = R (f x) rt (f y)

instance Relational Relation where
   leftHandSide  = lhs
   rightHandSide = rhs
   flipSides (R x rt y) = R y (flipRelType rt) x
   constructor (R _ rt _) x y = R x rt y

-- helpers
showRelType :: RelationType -> String
showRelType relType = fromMaybe "??" (lookup relType table)
 where
   table = [ (EqualTo, "=="), (NotEqualTo, "/="), (LessThan, "<")
           , (GreaterThan, ">"), (LessThanOrEqualTo, "<=")
           , (GreaterThanOrEqualTo, ">="), (Approximately, "~=")
           ]

flipRelType :: RelationType -> RelationType
flipRelType relType = fromMaybe relType (lookup relType table)
 where
   table = pairs ++ map (\(a,b) -> (b,a)) pairs
   pairs = [(LessThan, GreaterThan), (LessThanOrEqualTo, GreaterThanOrEqualTo)]

-----------------------------------------------------------------------------
-- Traversable instance declarations

instance Once   Relation where onceM  = onceMRelation
instance Switch Relation where switch = switchRelation
instance Crush  Relation where crush  = crushRelation

switchRelation :: (Relational f, Monad m) => f (m a) -> m (f a)
switchRelation p =
   liftM2 (constructor p) (leftHandSide p) (rightHandSide p)
 
onceMRelation :: (Relational f, MonadPlus m) => (a -> m a) -> f a -> m (f a)
onceMRelation f p =
   liftM (`updateLeft` p) (f (leftHandSide p)) `mplus` 
   liftM (`updateRight` p) (f (rightHandSide p))
            
crushRelation :: Relational f => f a -> [a]
crushRelation p = [leftHandSide p, rightHandSide p]

-----------------------------------------------------------------------------
-- QuickCheck generators

instance Arbitrary a => Arbitrary (Relation a) where
   arbitrary = liftM3 R arbitrary arbitrary arbitrary
   coarbitrary p = coarbitrary (relationType p) . coarbitrary (crush p)
   
instance Arbitrary RelationType where
   arbitrary = oneof $ map return [EqualTo .. Approximately]
   coarbitrary op = variant (fromEnum op)

-----------------------------------------------------------------------------
-- Constructor functions

infix 1 .==., ./=., .<., .>., .<=., .>=., .~=.

(.==.), (./=.), (.<.), (.>.), (.<=.), (.>=.), (.~=.) :: a -> a -> Relation a
(.==.) = makeType EqualTo
(./=.) = makeType NotEqualTo
(.<.)  = makeType LessThan 
(.>.)  = makeType GreaterThan 
(.<=.) = makeType LessThanOrEqualTo
(.>=.) = makeType GreaterThanOrEqualTo
(.~=.) = makeType Approximately

makeType :: RelationType -> a -> a -> Relation a
makeType = flip R

-----------------------------------------------------------------------------
-- Equation data type

infix 1 :==:

type Equations a = [Equation a]

data Equation  a = a :==: a
   deriving (Eq, Ord)

instance Show a => Show (Equation a) where
   show = show . build equationView

instance Functor Equation where
   fmap f (x :==: y) = f x :==: f y
   
instance Relational Equation where
   leftHandSide  = leftHandSide  . build equationView
   rightHandSide = rightHandSide . build equationView
   flipSides     = \(x :==: y) -> y :==: x
   constructor   = const (:==:)

instance Once   Equation where onceM  = onceMRelation
instance Switch Equation where switch = switchRelation
instance Crush  Equation where crush  = crushRelation

instance Arbitrary a => Arbitrary (Equation a) where
   arbitrary   = liftM2 (:==:) arbitrary arbitrary
   coarbitrary = coarbitrary . build equationView

equationView :: View (Relation a) (Equation a)
equationView = makeView f g
 where
   f (R x op y)
      | op == EqualTo = return (x :==: y)
      | otherwise     = Nothing
   g (x :==: y) = x .==. y
   
infix 1 :<:, :>:
   
data Inequality a = a :<: a | a :>: a