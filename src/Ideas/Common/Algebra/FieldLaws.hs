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

module Ideas.Common.Algebra.FieldLaws
   ( -- * Semi-ring laws
     leftDistributive, rightDistributive
   , distributiveLaws, semiRingLaws
     -- * Ring laws
   , leftNegateTimes, rightNegateTimes
   , negateTimesLaws, ringLaws, commutativeRingLaws
   , distributiveSubtractionLaws
     -- * Field laws
   , exchangeInverses, fieldLaws
     -- * Laws for additive monoid
   , fromAdditiveLaw
     -- * Laws for multiplicative monoid
   , fromMultiplicativeLaw
     -- * Properties
   , propsField
   ) where

import Ideas.Common.Algebra.Field
import Ideas.Common.Algebra.GroupLaws
import Ideas.Common.Algebra.Law
import Test.QuickCheck

--------------------------------------------------------
-- Semi-ring laws

leftDistributive :: SemiRing a => Law a
leftDistributive = leftDistributiveFor (|*|) (|+|)

rightDistributive :: SemiRing a => Law a
rightDistributive = rightDistributiveFor (|*|) (|+|)

distributiveLaws :: SemiRing a => [Law a]
distributiveLaws = [leftDistributive, rightDistributive]

semiRingLaws :: SemiRing a => [Law a]
semiRingLaws =
   map fromAdditiveLaw commutativeMonoidLaws ++
   map fromMultiplicativeLaw monoidZeroLaws ++
   distributiveLaws

--------------------------------------------------------
-- Ring laws

leftNegateTimes :: Ring a => Law a
leftNegateTimes = law "left-negate-times" $ \a b ->
   plusInverse a |*| b :==: plusInverse (a |*| b)

rightNegateTimes :: Ring a => Law a
rightNegateTimes = law "right-negate-times" $ \a b ->
   a |*| plusInverse b :==: plusInverse (a |*| b)

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
   [leftDistributiveFor (|*|) (|-|), rightDistributiveFor (|*|) (|-|)]

--------------------------------------------------------
-- Field laws

exchangeInverses :: Field a => Law a
exchangeInverses = law "exchange-inverses" $ \a ->
   timesInverse (plusInverse a) :==: plusInverse (timesInverse a)

fieldLaws :: Field a => [Law a]
fieldLaws =
   map fromAdditiveLaw abelianGroupLaws ++
   map fromMultiplicativeLaw abelianGroupLaws ++
   distributiveLaws ++ negateTimesLaws ++ [exchangeInverses]

--------------------------------------------------------
-- Laws for additive monoid

fromAdditiveLaw :: Law (Additive a) -> Law a
fromAdditiveLaw = mapLaw Additive fromAdditive

--------------------------------------------------------
-- Laws for multiplicative monoid

fromMultiplicativeLaw :: Law (Multiplicative a) -> Law a
fromMultiplicativeLaw = mapLaw Multiplicative fromMultiplicative

--------------------------------------------------------
-- Properties

propsField :: [Property]
propsField = map property (fieldLaws :: [Law (SafeNum Rational)])