{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Common.Algebra.Field
   ( -- * Semi-ring
     SemiRing(..)
     -- * Ring
   , Ring(..)
     -- * Field
   , Field(..)
     -- * Additive monoid
   , Additive(..)
     -- * Multiplicative monoid
   , Multiplicative(..)
     -- * Datatype for safe numeric operators
   , SafeNum, safeNum
     -- * CoSemiRing, CoRing, and CoField (for matching)
   , CoSemiRing(..), CoRing(..), CoField(..)
   ) where

import Control.Monad
import Ideas.Common.Algebra.Group
import Ideas.Common.Classes (mapBoth)
import Test.QuickCheck
import qualified Control.Applicative as A
import qualified Control.Applicative as Applicative

--------------------------------------------------------
-- Semi-ring

infixl 6 |+|
infixl 7 |*|

class SemiRing a where
   -- additive
   (|+|) :: a -> a -> a
   zero  :: a
   sum   :: [a] -> a
   -- multiplicative
   (|*|)   :: a -> a -> a
   one     :: a
   product :: [a] -> a
   -- default implementation
   sum     [] = zero
   sum     xs = foldl1 (|+|) xs
   product [] = one
   product xs = foldl1 (|*|) xs

--------------------------------------------------------
-- Ring

infixl 6 |-|

-- Minimal complete definition: plusInverse or <->
class SemiRing a => Ring a where
   plusInverse :: a -> a
   (|-|)       :: a -> a -> a
   -- default definitions
   plusInverse = (zero |-|)
   a |-| b     = a |+| plusInverse b

--------------------------------------------------------
-- Field

infixl 7 |/|

-- Minimal complete definition: mulInverse or </>
class Ring a => Field a where
   timesInverse :: a -> a
   (|/|)        :: a -> a -> a
   -- default definitions
   timesInverse = (one |/|)
   a |/| b      = a |*| timesInverse b

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
   mappend = A.liftA2 (|+|)

instance Ring a => Group (Additive a) where
   inverse   = A.liftA plusInverse
   appendInv = A.liftA2 (|-|)

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
   mappend = A.liftA2 (|*|)

instance Field a => Group (Multiplicative a) where
   inverse   = A.liftA timesInverse
   appendInv = A.liftA2 (|/|)

instance SemiRing a => MonoidZero (Multiplicative a) where
   mzero = Multiplicative zero

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

instance Applicative.Applicative SafeNum where
   pure  = return
   (<*>) = ap

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

instance (Eq a, Fractional a) => Fractional (SafeNum a) where
   a / b = liftM2 (/) a (safeDivisor b)
   recip = liftM recip . safeDivisor
   fromRational = return . fromRational

instance Num a => SemiRing (SafeNum a) where
   (|+|) = (+)
   (|*|) = (*)
   zero  = 0
   one   = 1

instance Num a => Ring (SafeNum a) where
   plusInverse = negate
   (|-|)       = (-)

instance (Eq a, Fractional a) => Field (SafeNum a) where
   timesInverse = recip
   (|/|)        = (/)

safeDivisor :: (Eq a, Num a) => SafeNum a -> SafeNum a
safeDivisor m = m >>= \a ->
   if a == 0 then fail "division by zero" else return a

------------------------------------------------------------

class CoSemiRing a where
   -- additive
   isPlus  :: a -> Maybe (a, a)
   isZero  :: a -> Bool
   -- multiplicative
   isTimes :: a -> Maybe (a, a)
   isOne   :: a -> Bool

-- Minimal complete definition: plusInverse or <->
class CoSemiRing a => CoRing a where
   isNegate :: a -> Maybe a
   isMinus  :: a -> Maybe (a, a)
   -- default definition
   isMinus _ = Nothing

class CoRing a => CoField a where
   isRecip    :: a -> Maybe a
   isDivision :: a -> Maybe (a, a)
   -- default definition
   isDivision _ = Nothing

instance CoSemiRing a => CoMonoid (Additive a) where
   isEmpty  = isZero . fromAdditive
   isAppend = fmap (mapBoth Additive) . isPlus . fromAdditive

instance CoRing a => CoGroup (Additive a) where
   isInverse   = fmap Additive . isNegate . fromAdditive
   isAppendInv = fmap (mapBoth Additive) . isMinus . fromAdditive

instance CoSemiRing a => CoMonoid (Multiplicative a) where
   isEmpty  = isOne . fromMultiplicative
   isAppend = fmap (mapBoth Multiplicative) . isTimes . fromMultiplicative

instance CoField a => CoGroup (Multiplicative a) where
   isInverse   = fmap Multiplicative . isRecip . fromMultiplicative
   isAppendInv = fmap (mapBoth Multiplicative) . isDivision . fromMultiplicative

instance CoSemiRing a => CoMonoidZero (Multiplicative a) where
   isMonoidZero = isZero . fromMultiplicative