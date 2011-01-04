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
-- Mathematical relations
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Relation 
   ( -- * Type class
     Relational(..), mapLeft, mapRight, updateLeft, updateRight
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

import Common.View
import Common.Rewriting hiding (Monoid)
import Common.Classes
import Data.Foldable (Foldable, foldMap, toList)
import Domain.Math.Expr.Symbols (openMathSymbol)
import Text.OpenMath.Dictionary.Relation1
import Data.Maybe
import Data.Monoid
import Test.QuickCheck
import Control.Monad

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

instance Foldable Relation where
   foldMap = foldMapRelation

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

instance Rewrite a => Rewrite (Relation a)

relationSymbols :: [(RelationType, (String, Symbol))]
relationSymbols =
   [ (EqualTo,              ("==", openMathSymbol eqSymbol))
   , (NotEqualTo,           ("/=", openMathSymbol neqSymbol))
   , (LessThan,             ("<",  openMathSymbol ltSymbol))
   , (GreaterThan,          (">",  openMathSymbol gtSymbol))
   , (LessThanOrEqualTo,    ("<=", openMathSymbol leqSymbol))
   , (GreaterThanOrEqualTo, (">=", openMathSymbol geqSymbol))
   , (Approximately,        ("~=", openMathSymbol approxSymbol))
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

eval :: Ord a => RelationType -> a -> a -> Bool
eval relType =
   case relType of
      EqualTo              -> (==)
      NotEqualTo           -> (/=)
      LessThan             -> (<)
      GreaterThan          -> (>)
      LessThanOrEqualTo    -> (<=)
      GreaterThanOrEqualTo -> (>=)
      Approximately        -> (==)

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

-----------------------------------------------------------------------------
-- Traversable instance declarations

instance Switch Relation where switch = switchRelation

switchRelation :: (Relational f, Monad m) => f (m a) -> m (f a)
switchRelation p =
   liftM2 (constructor p) (leftHandSide p) (rightHandSide p)

foldMapRelation :: (Relational f, Monoid m) => (a -> m) -> f a -> m
foldMapRelation f p = f (leftHandSide p) `mappend` f (rightHandSide p)

-----------------------------------------------------------------------------
-- QuickCheck generators

instance Arbitrary a => Arbitrary (Relation a) where
   arbitrary = liftM3 R arbitrary arbitrary arbitrary
   
instance CoArbitrary a => CoArbitrary (Relation a) where
   coarbitrary p = coarbitrary (relationType p) . coarbitrary (toList p)
   
instance Arbitrary RelationType where
   arbitrary = oneof $ map return [EqualTo .. Approximately]
   
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

instance Zip Equation where
   fzipWith f (a :==: b) (c :==: d) = f a c :==: f b d

instance Relational Equation where
   leftHandSide  = leftHandSide  . build equationView
   rightHandSide = rightHandSide . build equationView
   flipSides     = \(x :==: y) -> y :==: x
   constructor   = const (:==:)
   isSymmetric   = const True

instance Switch Equation where switch = switchRelation

instance Arbitrary a => Arbitrary (Equation a) where
   arbitrary   = liftM2 (:==:) arbitrary arbitrary
   
instance CoArbitrary a => CoArbitrary (Equation a) where
   coarbitrary = coarbitrary . build equationView

instance IsTerm a => IsTerm (Equation a) where
   toTerm = toTerm . build equationView
   fromTerm a = fromTerm a >>= matchM equationView

instance Rewrite a => Rewrite (Equation a)

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

instance Relational Inequality where
   leftHandSide  = leftHandSide  . build inequalityView
   rightHandSide = rightHandSide . build inequalityView
   flipSides = fromMaybe (error "inequality: flipSides") . matchM inequalityView 
             . flipSides . build inequalityView
   constructor ineq = 
      let relType = relationType (build inequalityView ineq)
      in fst (relType ? inequalityTable)

instance Switch Inequality where switch = switchRelation

instance Arbitrary a => Arbitrary (Inequality a) where
   arbitrary = do 
      op <- oneof $ map (return . fst . snd) inequalityTable
      liftM2 op arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Inequality a) where
   coarbitrary = coarbitrary . build inequalityView

instance IsTerm a => IsTerm (Inequality a) where
   toTerm = toTerm . build inequalityView
   fromTerm a = fromTerm a >>= matchM inequalityView

instance Rewrite a => Rewrite (Inequality a)

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