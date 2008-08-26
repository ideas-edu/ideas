module Domain.Math.Constrained (Constrained, Con, fromConstrained) where

import Common.Context 
import Control.Monad
import Domain.Math.Classes
import Data.Monoid
import Test.QuickCheck

-----------------------------------------------------------------------
-- Constrained values

data Constrained c a = C (Prop c) a 
   deriving (Show, Eq)

instance Functor (Constrained c) where
   fmap f (C p a) = C p (f a)

instance Monad (Constrained c) where
   return = C mempty
   C p a >>= f = let C q b = f a
                 in C (p /\ q) b

constrain :: Prop c -> Constrained c ()
constrain p = C p ()

infixl 2 #

(#) :: Constrained c a -> Prop c -> Constrained c a
c # p = constrain p >> c

-----------------------------------------------------------------------
-- Propositions

data Prop a = T | F | Not (Prop a) | Prop a :/\: Prop a | Prop a :\/: Prop a | Atom a
   deriving (Show, Eq)

instance Functor Prop where
   fmap f prop = 
      case prop of
         T        -> T
         F        -> F
         Not p    -> Not (fmap f p)
         p :/\: q -> fmap f p :/\: fmap f q
         p :\/: q -> fmap f p :\/: fmap f q

instance Monoid (Prop a) where
   mempty  = T
   mappend = (/\)

-- smart constructor
(/\) :: Prop a -> Prop a -> Prop a
T /\ p = p
p /\ T = p
F /\ _ = F
_ /\ F = F
p /\ q = p :/\: q

-----------------------------------------------------------------------
-- Elementary constraints (implied by sqrt and /)
 
data Con a = a :==: a   -- equality
           | a :<:  a   -- ordering
           | WF a       -- well-formedness
   deriving (Show, Eq)

instance Functor Con where
   fmap f con =
      case con of
         x :==: y -> f x :==: f y
         x :<:  y -> f x :<:  f y
         WF x     -> WF (f x)

-----------------------------------------------------------------------
-- Numeric instances

instance (Show c, Eq c, Num a) => Num (Constrained c a) where
   (+) = liftM2 (+)
   (*) = liftM2 (*)
   (-) = liftM2 (-)
   negate      = liftM negate
   fromInteger = return . fromInteger

instance (Show c, Eq c, Fractional a) => Fractional (Constrained c a) where
   (/) = liftM2 (/)
   fromRational = return . fromRational
   
instance (Show c, Eq c, Floating a) => Floating (Constrained c a) where
   sqrt = liftM sqrt
   pi   = return pi
   
instance (Show c, Eq c, Symbolic a) => Symbolic (Constrained c a) where
   variable   = return . variable
   function s = liftM (function s) . sequence

-----------------------------------------------------------------------
-- Various instances

{- instance Arbitrary a => Arbitrary (Constrained a) where
   arbitrary = liftM toConstrained arbitrary
   coarbitrary (C a x) = coarbitrary a . coarbitrary x

instance Arbitrary (Prop a) 

instance Uniplate (Constrained a) 

instance UniplateConstr (Constrained a) 

instance ShallowEq (Constrained a)

instance Constructor (Constrained a)

instance MetaVar (Constrained a) -}

-----------------------------------------------------------------------
-- Remaining functions

fromConstrained :: Constrained c a -> a
fromConstrained (C _ a) = a

proposition :: Constrained c a -> Prop c
proposition (C a _) = a

infix 3 #==, #<, #/=, #>=

wf :: a -> Prop (Con a)
wf = Atom . WF

(#==), (#<), (#/=), (#>=) :: a -> a -> Prop (Con a)
a #== b = Atom $ a :==: b
a #<  b = Atom $ a :<:  b
a #/= b = Not $ Atom $ a :==: b
a #>= b = Not $ Atom $ a :<: b