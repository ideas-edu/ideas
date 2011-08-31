-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( -- * Type class
     Relational(..)
     -- * Relation data type
   , Relation, relationType, RelationType(..), relationSymbols
   , notRelation, eval
     -- * Constructor functions
   , makeType, (.==.), (./=.), (.<.), (.>.), (.<=.), (.>=.), (.~=.)
     -- * Equation (or equality)
   , Equations, Equation(..), equationView
     -- * Inequality
   , Inequality(..), inequalityView
   ) where

import Common.Rewriting
import Common.View
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable, foldMap, toList)
import Data.Maybe
import Data.Monoid
import Data.Traversable (Traversable, sequenceA)
import Test.QuickCheck
import Text.OpenMath.Dictionary.Relation1

-----------------------------------------------------------------------------
-- Type class for relations

class Functor f => Relational f where
   leftHandSide  :: f a -> a
   rightHandSide :: f a -> a
   flipSides     :: f a -> f a -- possibly also flips operator
   constructor   :: f a -> b -> b -> f b
   isSymmetric   :: f a -> Bool
   -- default definitions
   isSymmetric _ = False

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

instance Foldable Relation where
   foldMap = foldMapRelation

instance Traversable Relation where
   sequenceA = sequenceRelation

instance Relational Relation where
   leftHandSide  = lhs
   rightHandSide = rhs
   flipSides (R x rt y) = R y (flipRelType rt) x
   constructor (R _ rt _) = flip R rt
   isSymmetric = (`elem` [EqualTo, NotEqualTo, Approximately]) . relationType

instance IsTerm a => IsTerm (Relation a) where
   toTerm p =
      let op  = relationType p
          sym = maybe (newSymbol (show op)) snd (lookup op relationSymbols)
      in binary sym (toTerm (leftHandSide p)) (toTerm (rightHandSide p))
   fromTerm term =
      case getFunction term of
         Just (s, [a, b]) ->
            case [ rt | (rt, (_, t)) <- relationSymbols, s==t ] of
               [rt] -> liftM2 (makeType rt) (fromTerm a) (fromTerm b)
               _    -> fail "fromTerm: relation"
         _ -> fail "fromTerm: relation"

relationSymbols :: [(RelationType, (String, Symbol))]
relationSymbols =
   [ (EqualTo,              ("==", newSymbol eqSymbol))
   , (NotEqualTo,           ("/=", newSymbol neqSymbol))
   , (LessThan,             ("<",  newSymbol ltSymbol))
   , (GreaterThan,          (">",  newSymbol gtSymbol))
   , (LessThanOrEqualTo,    ("<=", newSymbol leqSymbol))
   , (GreaterThanOrEqualTo, (">=", newSymbol geqSymbol))
   , (Approximately,        ("~=", newSymbol approxSymbol))
   ]

notRelation :: Relation a -> Relation a
notRelation r = r { relationType = relationType r ? table }
 where
   table = xs ++ map swap xs ++ [(Approximately, Approximately)]
   swap (x, y) = (y, x)
   xs = [ (EqualTo, NotEqualTo)
        , (LessThan, GreaterThanOrEqualTo)
        , (LessThanOrEqualTo, GreaterThan)
        ]

eval :: (Ord a, Num a) => RelationType -> a -> a -> Bool
eval relType =
   case relType of
      EqualTo              -> (==)
      NotEqualTo           -> (/=)
      LessThan             -> (<)
      GreaterThan          -> (>)
      LessThanOrEqualTo    -> (<=)
      GreaterThanOrEqualTo -> (>=)
      Approximately        -> \a b -> 1000 * abs (a-b) < 1

-- helpers
showRelType :: RelationType -> String
showRelType = fst . (? relationSymbols)

flipRelType :: RelationType -> RelationType
flipRelType relType = fromMaybe relType (lookup relType table)
 where
   table = pairs ++ map (\(a,b) -> (b,a)) pairs
   pairs = [(LessThan, GreaterThan), (LessThanOrEqualTo, GreaterThanOrEqualTo)]

(?) :: Eq a => a -> [(a, b)] -> b
a ? xs = fromMaybe (error "Relation: Error in lookup") (lookup a xs)

foldMapRelation :: (Relational f, Monoid m) => (a -> m) -> f a -> m
foldMapRelation f p = f (leftHandSide p) `mappend` f (rightHandSide p)

sequenceRelation :: (Relational g, Applicative f) => g (f a) -> f (g a)
sequenceRelation p = constructor p <$> leftHandSide p <*> rightHandSide p

-----------------------------------------------------------------------------
-- QuickCheck generators

instance Arbitrary a => Arbitrary (Relation a) where
   arbitrary = liftM3 R arbitrary arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Relation a) where
   coarbitrary p = coarbitrary (relationType p) . coarbitrary (toList p)

instance Arbitrary RelationType where
   arbitrary = elements [EqualTo .. Approximately]

instance CoArbitrary RelationType where
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
-- Equation data type (view on Relation)

infix 1 :==:

type Equations a = [Equation a]

data Equation  a = a :==: a
   deriving (Eq, Ord)

instance Show a => Show (Equation a) where
   show = show . build equationView

instance Functor Equation where
   fmap f (x :==: y) = f x :==: f y

instance Foldable Equation where
   foldMap = foldMapRelation

instance Traversable Equation where
   sequenceA = sequenceRelation

instance Relational Equation where
   leftHandSide  = leftHandSide  . build equationView
   rightHandSide = rightHandSide . build equationView
   flipSides (x :==: y) = y :==: x
   constructor   = const (:==:)
   isSymmetric   = const True

instance Arbitrary a => Arbitrary (Equation a) where
   arbitrary   = liftM2 (:==:) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Equation a) where
   coarbitrary = coarbitrary . build equationView

instance IsTerm a => IsTerm (Equation a) where
   toTerm = toTerm . build equationView
   fromTerm a = fromTerm a >>= matchM equationView

equationView :: View (Relation a) (Equation a)
equationView = makeView f g
 where
   f (R x op y)
      | op == EqualTo = return (x :==: y)
      | otherwise     = Nothing
   g (x :==: y) = x .==. y

-----------------------------------------------------------------------------
-- Inequality (view on Relation)

infix 1 :<:, :>:, :<=:, :>=:

data Inequality a = a :<: a | a :>: a | a :<=: a | a :>=: a

instance Show a => Show (Inequality a) where
   show = show . build inequalityView

instance Functor Inequality where
   fmap f ineq =
      let a = leftHandSide ineq
          b = rightHandSide ineq
      in constructor ineq (f a) (f b)

instance Foldable Inequality where
   foldMap = foldMapRelation

instance Traversable Inequality where
   sequenceA = sequenceRelation

instance Relational Inequality where
   leftHandSide  = leftHandSide  . build inequalityView
   rightHandSide = rightHandSide . build inequalityView
   flipSides = fromMaybe (error "inequality: flipSides") . matchM inequalityView
             . flipSides . build inequalityView
   constructor ineq =
      let relType = relationType (build inequalityView ineq)
      in fst (relType ? inequalityTable)

instance Arbitrary a => Arbitrary (Inequality a) where
   arbitrary = do
      op <- elements $ map (fst . snd) inequalityTable
      liftM2 op arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Inequality a) where
   coarbitrary = coarbitrary . build inequalityView

instance IsTerm a => IsTerm (Inequality a) where
   toTerm = toTerm . build inequalityView
   fromTerm a = fromTerm a >>= matchM inequalityView

inequalityView :: View (Relation a) (Inequality a)
inequalityView = makeView f g
 where
   f (R x op y) = fmap (\pair -> fst pair x y) (lookup op inequalityTable)
   g ineq =
      case ineq of
         x :<:  y -> x .<.  y
         x :>:  y -> x .>.  y
         x :<=: y -> x .<=. y
         x :>=: y -> x .>=. y

inequalityTable :: [(RelationType, (a -> a -> Inequality a, a -> a -> Relation a))]
inequalityTable =
   [ (LessThan, ((:<:), (.<.))), (LessThanOrEqualTo, ((:<=:), (.<=.)))
   , (GreaterThan, ((:>:), (.>.))), (GreaterThanOrEqualTo, ((:>=:), (.>=.)))
   ]