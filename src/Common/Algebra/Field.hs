{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
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
-----------------------------------------------------------------------------
module Common.Algebra.Field 
   ( -- * Semi-ring
     SemiRing(..), leftDistributive, rightDistributive
   , distributiveLaws, semiRingLaws
     -- * Ring
   , Ring(..), leftNegateTimes, rightNegateTimes
   , negateTimesLaws, ringLaws, commutativeRingLaws
   , distributiveSubtractionLaws
     -- * Field
   , Field(..), exchangeInverses, fieldLaws
     -- * Additive monoid
   , Additive(..), fromAdditiveLaw
     -- * Multiplicative monoid
   , Multiplicative(..), fromMultiplicativeLaw
     -- * Datatype for safe numeric operators
   , SafeNum, safeNum
   , propsField
   ) where

import Common.Algebra.Law
import Common.Algebra.Group
import Control.Monad
import qualified Control.Applicative as A
import Test.QuickCheck

--------------------------------------------------------
-- Semi-ring

infixr 6 <+>
infixr 7 <*>

class SemiRing a where
   -- additive
   (<+>)        :: a -> a -> a
   plusIdentity :: a
   -- multiplicative
   (<*>)        :: a -> a -> a
   mulIdentity  :: a

leftDistributive :: SemiRing a => Law a
leftDistributive = leftDistributiveFor (<*>) (<+>)

rightDistributive :: SemiRing a => Law a
rightDistributive = rightDistributiveFor (<*>) (<+>)

distributiveLaws :: SemiRing a => [Law a]
distributiveLaws = [leftDistributive, rightDistributive]

semiRingLaws :: SemiRing a => [Law a]
semiRingLaws = 
   map fromAdditiveLaw commutativeMonoidLaws ++
   map fromMultiplicativeLaw monoidZeroLaws ++
   distributiveLaws

--------------------------------------------------------
-- Ring

-- Minimal complete definition: plusInverse or <->
class SemiRing a => Ring a where
   plusInverse :: a -> a
   (<->)       :: a -> a -> a
   -- default definitions
   plusInverse = (plusIdentity <->)
   a <-> b     = a <+> plusInverse b

leftNegateTimes :: Ring a => Law a
leftNegateTimes = law "left-negate-times" $ \a b -> 
   plusInverse a <*> b :==: plusInverse (a <*> b)

rightNegateTimes :: Ring a => Law a
rightNegateTimes = law "right-negate-times" $ \a b -> 
   a <*> plusInverse b :==: plusInverse (a <*> b)

negateTimesLaws :: Ring a => [Law a]
negateTimesLaws = [leftNegateTimes, rightNegateTimes]

ringLaws :: Ring a => [Law a]
ringLaws = 
   map fromAdditiveLaw abelianGroupLaws ++
   map fromMultiplicativeLaw monoidZeroLaws ++
   distributiveLaws ++ negateTimesLaws

commutativeRingLaws :: Ring a => [Law a]
commutativeRingLaws = 
   fromMultiplicativeLaw commutative : ringLaws

distributiveSubtractionLaws :: Ring a => [Law a]
distributiveSubtractionLaws = 
   [leftDistributiveFor (<*>) (<->), rightDistributiveFor (<*>) (<->)]

--------------------------------------------------------
-- Field
   
-- Minimal complete definition: mulInverse or </>
class Ring a => Field a where
   mulInverse :: a -> a
   (</>)      :: a -> a -> a
   -- default definitions
   mulInverse = (mulIdentity </>)
   a </> b    = a <*> mulInverse b

exchangeInverses :: Field a => Law a 
exchangeInverses = law "exchange-inverses" $ \a -> 
   mulInverse (plusInverse a) :==: plusInverse (mulInverse a)

fieldLaws :: Field a => [Law a]
fieldLaws =
   map fromAdditiveLaw abelianGroupLaws ++
   map fromMultiplicativeLaw abelianGroupLaws ++
   distributiveLaws ++ negateTimesLaws ++ [exchangeInverses]

--------------------------------------------------------
-- Additive monoid

newtype Additive a = Additive {fromAdditive :: a}
   deriving (Show, Eq, Ord, Functor, Arbitrary, CoArbitrary)

instance A.Applicative Additive where
   pure = Additive
   Additive f <*> Additive a = Additive (f a)

instance SemiRing a => Monoid (Additive a) where
   mempty  = A.pure plusIdentity
   mappend = A.liftA2 (<+>)
 
instance Ring a => Group (Additive a) where
   inverse       = A.liftA plusInverse
   appendInverse = A.liftA2 (<->)

fromAdditiveLaw :: Law (Additive a) -> Law a
fromAdditiveLaw = mapLaw Additive fromAdditive

--------------------------------------------------------
-- Multiplicative monoid

newtype Multiplicative a = Multiplicative {fromMultiplicative :: a}
   deriving (Show, Eq, Ord, Functor, Arbitrary, CoArbitrary)

instance A.Applicative Multiplicative where
   pure = Multiplicative
   Multiplicative f <*> Multiplicative a = Multiplicative (f a)

instance SemiRing a => Monoid (Multiplicative a) where
   mempty  = A.pure mulIdentity
   mappend = A.liftA2 (<*>)
   
instance Field a => Group (Multiplicative a) where
   inverse       = A.liftA mulInverse
   appendInverse = A.liftA2 (</>)
   
instance SemiRing a => MonoidZero (Multiplicative a) where 
   zero = Multiplicative plusIdentity

fromMultiplicativeLaw :: Law (Multiplicative a) -> Law a
fromMultiplicativeLaw = mapLaw Multiplicative fromMultiplicative

--------------------------------------------------------
-- Datatype for safe numeric operators

data SafeNum a = Ok a | Exception String

safeNum :: SafeNum a -> Either String a
safeNum (Ok a)        = Right a
safeNum (Exception s) = Left s

instance Arbitrary a => Arbitrary (SafeNum a) where
   arbitrary = liftM return arbitrary

instance Eq a => Eq (SafeNum a) where
   Ok a == Ok b = a == b
   _    == _    = True

instance Show a => Show (SafeNum a) where
   show = either ("Exception: " ++) show . safeNum

instance Functor SafeNum where
   fmap f = either Exception (return . f) . safeNum
  
instance Monad SafeNum where
   return  = Ok
   fail    = Exception
   m >>= f = either Exception f (safeNum m)
   
instance Num a => Num (SafeNum a) where
   (+) = liftM2 (+)
   (*) = liftM2 (*)
   (-) = liftM2 (-)
   negate = liftM negate
   abs    = liftM abs
   signum = liftM signum
   fromInteger = return . fromInteger
   
instance Fractional a => Fractional (SafeNum a) where
   a / b = liftM2 (/) a (safeDivisor b)
   recip = liftM recip . safeDivisor
   fromRational = return . fromRational

instance Num a => SemiRing (SafeNum a) where
   (<+>)        = (+)
   (<*>)        = (*)
   plusIdentity = 0
   mulIdentity  = 1
   
instance Num a => Ring (SafeNum a) where
   plusInverse = negate
   (<->)       = (-)
   
instance Fractional a => Field (SafeNum a) where
   mulInverse = recip
   (</>)      = (/)
   
safeDivisor :: Num a => SafeNum a -> SafeNum a
safeDivisor m = m >>= \a -> 
   if a == 0 then fail "division by zero" else return a
   
propsField :: [Property]
propsField = map property (fieldLaws :: [Law (SafeNum Rational)]) 