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

module Ideas.Common.Algebra.GroupLaws
   ( -- * Monoid laws
     associative, leftIdentity
   , rightIdentity, identityLaws, monoidLaws, commutativeMonoidLaws
   , idempotent
     -- * Group laws
   , leftInverse, rightInverse, doubleInverse
   , inverseIdentity, inverseDistrFlipped, inverseLaws, groupLaws
   , appendInverseLaws
     -- * Abelian group laws
   , commutative, inverseDistr, abelianGroupLaws
     -- * Laws for monoids with a zero element
   , leftZero, rightZero, zeroLaws, monoidZeroLaws
     -- * Generalized laws
   , associativeFor, commutativeFor, idempotentFor
   , leftDistributiveFor, rightDistributiveFor
   ) where

import Prelude hiding ((<*>))
import Ideas.Common.Algebra.Group
import Ideas.Common.Algebra.Law

--------------------------------------------------------
-- Monoids

associative :: Monoid a => Law a
associative = associativeFor (<>)

leftIdentity :: Monoid a => Law a
leftIdentity = law "left-identity" $ \a -> mempty <> a :==: a

rightIdentity :: Monoid a => Law a
rightIdentity = law "right-identity" $ \a -> a <> mempty :==: a

identityLaws :: Monoid a => [Law a]
identityLaws = [leftIdentity, rightIdentity]

monoidLaws :: Monoid a => [Law a]
monoidLaws = associative : identityLaws

commutativeMonoidLaws :: Monoid a => [Law a]
commutativeMonoidLaws = monoidLaws ++ [commutative]

-- | Not all monoids are idempotent (see: idempotentFor)
idempotent :: Monoid a => Law a
idempotent = idempotentFor (<>)

--------------------------------------------------------
-- Groups

leftInverse :: Group a => Law a
leftInverse = law "left-inverse" $ \a -> inverse a <> a :==: mempty

rightInverse :: Group a => Law a
rightInverse = law "right-inverse" $ \a -> a <> inverse a :==: mempty

doubleInverse :: Group a => Law a
doubleInverse = law "double-inverse" $ \a -> inverse (inverse a) :==: a

inverseIdentity :: Group a => Law a
inverseIdentity = law "inverse-identity" $ inverse mempty :==: mempty

inverseDistrFlipped :: Group a => Law a
inverseDistrFlipped = law "inverse-distr-flipped" $ \a b ->
   inverse (a <> b) :==: inverse b <> inverse a

inverseLaws :: Group a => [Law a]
inverseLaws = [leftInverse, rightInverse]

groupLaws :: Group a => [Law a]
groupLaws = monoidLaws ++ inverseLaws ++
   [doubleInverse, inverseIdentity, inverseDistrFlipped]

appendInverseLaws :: Group a => [Law a]
appendInverseLaws =
   [ make 1 $ \a b   ->           a <>- b :==: a <> inverse b
   , make 2 $ \a     ->           a <>- a :==: mempty
   , make 3 $ \a     ->      a <>- mempty :==: a
   , make 4 $ \a     ->      mempty <>- a :==: inverse a
   , make 5 $ \a b c ->    a <>- (b <> c) :==: (a <>- b) <>- c
   , make 6 $ \a b c ->   a <>- (b <>- c) :==: (a <>- b) <> c
   , make 7 $ \a b c ->    a <> (b <>- c) :==: (a <> b) <>- c
   , make 8 $ \a b   ->   a <>- inverse b :==: a <> b
   , make 9 $ \a b   -> inverse (a <>- b) :==: inverse a <> b
   ]
 where
    make n = law ("append-inverse-law" ++ show (n :: Int))

--------------------------------------------------------
-- Abelian groups

commutative :: Monoid a => Law a
commutative = commutativeFor (<>)

inverseDistr :: Group a => Law a
inverseDistr = law "inverse-distr" $ \a b ->
    inverse (a <> b) :==: (inverse a <> inverse b)

abelianGroupLaws :: Group a => [Law a]
abelianGroupLaws = groupLaws ++ [commutative, inverseDistr]

--------------------------------------------------------
-- Monoids with a zero element
-- This element could be the additive identity from a (semi-)ring for
-- the multiplicative monoid

leftZero :: MonoidZero a => Law a
leftZero = law "left-zero" $ \a -> mzero <> a :==: mzero

rightZero:: MonoidZero a => Law a
rightZero = law "right-zero" $ \a -> a <> mzero :==: mzero

zeroLaws :: MonoidZero a => [Law a]
zeroLaws = [leftZero, rightZero]

monoidZeroLaws :: MonoidZero a => [Law a]
monoidZeroLaws = monoidLaws ++ zeroLaws

--------------------------------------------------------
-- Generalized laws

associativeFor :: (a -> a -> a) -> Law a
associativeFor (?) = law "associative" $ \a b c ->
   a ? (b ? c) :==: (a ? b) ? c

commutativeFor :: (a -> a -> a) -> Law a
commutativeFor (?) = law "commutative" $ \a b -> a ? b :==: b ? a

idempotentFor :: (a -> a -> a) -> Law a
idempotentFor (?) = law "idempotent" $ \a -> a ? a :==: a

leftDistributiveFor :: (a -> a -> a) -> (a -> a -> a) -> Law a
leftDistributiveFor (<*>) (<+>) = law "left-distributive" $ \a b c ->
   a <*> (b <+> c) :==: (a <*> b) <+> (a <*> c)

rightDistributiveFor :: (a -> a -> a) -> (a -> a -> a) -> Law a
rightDistributiveFor (<*>) (<+>) = law "right-distributive" $ \a b c ->
   (a <+> b) <*> c :==: (a <*> c) <+> (b <*> c)
