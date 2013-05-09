{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
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
module Ideas.Common.Algebra.CoField
   ( CoSemiRing(..), CoRing(..), CoField(..)
   , SmartField(..)
   , (.+.), (.-.), neg, (.*.), (./.)
   ) where

import Ideas.Common.Algebra.CoGroup
import Ideas.Common.Algebra.Field
import Ideas.Common.Algebra.Group
import Ideas.Common.Algebra.SmartGroup
import Control.Arrow ((***))
import qualified Control.Applicative as A

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
   isAppend = fmap (Additive *** Additive) . isPlus . fromAdditive

instance CoRing a => CoGroup (Additive a) where
   isInverse   = fmap Additive . isNegate . fromAdditive
   isAppendInv = fmap (Additive *** Additive) . isMinus . fromAdditive

instance CoSemiRing a => CoMonoid (Multiplicative a) where
   isEmpty  = isOne . fromMultiplicative
   isAppend = fmap (Multiplicative *** Multiplicative) . isTimes . fromMultiplicative

instance CoField a => CoGroup (Multiplicative a) where
   isInverse   = fmap Multiplicative . isRecip . fromMultiplicative
   isAppendInv = fmap (Multiplicative *** Multiplicative) . isDivision . fromMultiplicative

instance CoSemiRing a => CoMonoidZero (Multiplicative a) where
   isMonoidZero = isZero . fromMultiplicative

------------------------------------------------------------------

newtype SmartField a = SmartField {fromSmartField :: a}
   deriving (CoSemiRing, CoRing, CoField)

instance Functor SmartField where -- could be derived
   fmap f = SmartField . f . fromSmartField

instance A.Applicative SmartField where
   pure = SmartField
   SmartField f <*> SmartField a = SmartField (f a)

instance (CoField a, Field a) => SemiRing (SmartField a) where
   zero = SmartField zero
   one  = SmartField one
   SmartField a <+> SmartField b = SmartField $ fromAdditive $ fromSmartGroup $
      SmartGroup (Additive a) <> SmartGroup (Additive b)
   a <*> b
      | Just x <- isNegate a = plusInverse (x <*> b)
      | Just x <- isNegate b = plusInverse (a <*> x)
      | isZero a || isZero b = zero
      | isOne a = b
      | isOne b = a
      | Just (x, y) <- isTimes b = (a <*> x) <*> y
      | Just (x, y) <- isDivision b = (a <*> x) </> y
      | otherwise = A.liftA2 (<*>) a b

instance (CoField a, Field a) => Ring (SmartField a) where
   plusInverse = SmartField . fromAdditive . fromSmartGroup . inverse
               . SmartGroup . Additive . fromSmartField
   SmartField a <-> SmartField b = SmartField $ fromAdditive $ fromSmartGroup $
      SmartGroup (Additive a) <>- SmartGroup (Additive b)

instance (CoField a, Field a) => Field (SmartField a) where
   timesInverse a
      | Just x <- isNegate a = plusInverse (timesInverse x)
      | Just (x, y) <- isDivision a, isOne y = x
      | otherwise = A.liftA timesInverse a
   a </> b
      | Just x <- isNegate a = plusInverse (x </> b)
      | Just x <- isNegate b = plusInverse (a </> x)
      | isOne b = a
      | Just (x, y) <- isDivision a = x </> (y <*> b)
      | otherwise = A.liftA2 (</>) a b

------------------------------------------------------------------

infixl 7 .*., ./.
infixl 6 .-., .+.

(.+.) :: (CoField a, Field a) => a -> a -> a
a .+. b = fromSmartField $ SmartField a <+> SmartField b

(.-.) :: (CoField a, Field a) => a -> a -> a
a .-. b = fromSmartField $ SmartField a <-> SmartField b

neg :: (CoField a, Field a) => a -> a
neg = fromSmartField . plusInverse . SmartField

(.*.) :: (CoField a, Field a) => a -> a -> a
a .*. b = fromSmartField $ SmartField a <*> SmartField b

(./.) :: (CoField a, Field a) => a -> a -> a
a ./. b = fromSmartField $ SmartField a </> SmartField b

-- myrecip :: (CoField a, Field a) => a -> a
-- myrecip = fromSmartField . timesInverse . SmartField