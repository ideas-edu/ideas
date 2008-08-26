module Domain.Math.Constrained (Constrained, toConstrained, fromConstrained, liftC) where

import Common.Context 
import Control.Monad
import Domain.Math.Classes
import Data.Monoid
import Test.QuickCheck

-----------------------------------------------------------------------
-- Constrained values

data Constrained a = C (Prop (Con a)) a 
   deriving (Show, Eq)

-----------------------------------------------------------------------
-- Propositions

data Prop a = T | F | Not (Prop a) | Prop a :/\: Prop a | Prop a :\/: Prop a | Atom a
   deriving (Show, Eq)

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

-----------------------------------------------------------------------
-- Numeric instances

instance Num a => Num (Constrained a) where
   (+) = liftC2 (+)
   (*) = liftC2 (*)
   (-) = liftC2 (-)
   negate      = liftC negate
   fromInteger = toConstrained . fromInteger

instance Fractional a => Fractional (Constrained a) where
   (/) = liftC2 (/)
   fromRational = toConstrained . fromRational
   
instance Floating a => Floating (Constrained a) where
   sqrt = liftC sqrt
   pi   = toConstrained pi
   
instance Symbolic a => Symbolic (Constrained a) where
   variable   = toConstrained . variable
   function s = liftCs (function s)

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

liftC  f (C a x)         = C a (f x)
liftC2 f (C a x) (C b y) = C (a `mappend` b) (f x y)

liftCs f xs = C (mconcat $ map proposition xs) (f $ map fromConstrained xs)

toConstrained :: a -> Constrained a
toConstrained = C mempty

fromConstrained :: Constrained a -> a
fromConstrained (C _ a) = a

proposition :: Constrained a -> Prop (Con a)
proposition (C a _) = a

infixl 2 #

(#) :: Constrained a -> Prop (Con a) -> Constrained a
C p a # q = C (p /\ q) a

infix 3 #==, #<, #/=, #>=

wf :: a -> Prop (Con a)
wf = Atom . WF

(#==), (#<), (#/=), (#>=) :: a -> a -> Prop (Con a)
a #== b = Atom $ a :==: b
a #<  b = Atom $ a :<:  b
a #/= b = Not $ Atom $ a :==: b
a #>= b = Not $ Atom $ a :<: b