{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Common.Algebra.Group
import Common.Algebra.Law
import Control.Monad
import Test.QuickCheck
import qualified Control.Applicative as A

--------------------------------------------------------
-- Semi-ring

infixl 6 <+>
infixl 7 <*>

class SemiRing a where
   -- additive
   (<+>) :: a -> a -> a
   zero  :: a
   -- multiplicative
   (<*>) :: a -> a -> a
   one   :: a

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

infixl 6 <->

-- Minimal complete definition: plusInverse or <->
class SemiRing a => Ring a where
   plusInverse :: a -> a
   (<->)       :: a -> a -> a
   -- default definitions
   plusInverse = (zero <->)
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

infixl 7 </>

-- Minimal complete definition: mulInverse or </>
class Ring a => Field a where
   timesInverse :: a -> a
   (</>)        :: a -> a -> a
   -- default definitions
   timesInverse = (one </>)
   a </> b      = a <*> timesInverse b

exchangeInverses :: Field a => Law a
exchangeInverses = law "exchange-inverses" $ \a ->
   timesInverse (plusInverse a) :==: plusInverse (timesInverse a)

fieldLaws :: Field a => [Law a]
fieldLaws =
   map fromAdditiveLaw abelianGroupLaws ++
   map fromMultiplicativeLaw abelianGroupLaws ++
   distributiveLaws ++ negateTimesLaws ++ [exchangeInverses]

--------------------------------------------------------
-- Additive monoid

newtype Additive a = Additive {fromAdditive :: a}
   deriving (Show, Eq, Ord, Arbitrary, CoArbitrary)

instance Functor Additive where -- could be derived
   fmap f = Additive . f . fromAdditive

instance A.Applicative Additive where
   pure = Additive
   Additive f <*> Additive a = Additive (f a)

instance SemiRing a => Monoid (Additive a) where
   mempty  = A.pure zero
   mappend = A.liftA2 (<+>)

instance Ring a => Group (Additive a) where
   inverse   = A.liftA plusInverse
   appendInv = A.liftA2 (<->)

fromAdditiveLaw :: Law (Additive a) -> Law a
fromAdditiveLaw = mapLaw Additive fromAdditive

--------------------------------------------------------
-- Multiplicative monoid

newtype Multiplicative a = Multiplicative {fromMultiplicative :: a}
   deriving (Show, Eq, Ord, Arbitrary, CoArbitrary)

instance Functor Multiplicative where -- could be derived
   fmap f = Multiplicative . f . fromMultiplicative

instance A.Applicative Multiplicative where
   pure = Multiplicative
   Multiplicative f <*> Multiplicative a = Multiplicative (f a)

instance SemiRing a => Monoid (Multiplicative a) where
   mempty  = A.pure one
   mappend = A.liftA2 (<*>)

instance Field a => Group (Multiplicative a) where
   inverse   = A.liftA timesInverse
   appendInv = A.liftA2 (</>)

instance SemiRing a => MonoidZero (Multiplicative a) where
   mzero = Multiplicative zero

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

instance Ord a => Ord (SafeNum a) where
   Ok a `compare` Ok b = a `compare` b
   _    `compare` _    = EQ

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
   (<+>) = (+)
   (<*>) = (*)
   zero  = 0
   one   = 1

instance Num a => Ring (SafeNum a) where
   plusInverse = negate
   (<->)       = (-)

instance Fractional a => Field (SafeNum a) where
   timesInverse = recip
   (</>)        = (/)

safeDivisor :: Num a => SafeNum a -> SafeNum a
safeDivisor m = m >>= \a ->
   if a == 0 then fail "division by zero" else return a

propsField :: [Property]
propsField = map property (fieldLaws :: [Law (SafeNum Rational)])