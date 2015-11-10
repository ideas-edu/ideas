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

module Ideas.Common.Algebra.BooleanLaws
   ( -- * Boolean laws
     andOverOrLaws, orOverAndLaws
   , complementAndLaws, complementOrLaws
   , absorptionAndLaws, absorptionOrLaws
   , deMorganAnd, deMorganOr
   , doubleComplement, complementTrue, complementFalse
   , booleanLaws
     -- * Law transformer
   , fromAndLaw, fromOrLaw
     -- * Properties
   , propsBoolean
   ) where

import Ideas.Common.Algebra.Boolean
import Ideas.Common.Algebra.Group
import Ideas.Common.Algebra.GroupLaws
import Ideas.Common.Algebra.Law
import Test.QuickCheck hiding ((><))

--------------------------------------------------------
-- Boolean laws

andOverOrLaws, orOverAndLaws :: Boolean a => [Law a]
andOverOrLaws = map fromAndLaw dualDistributive
orOverAndLaws = map fromOrLaw  dualDistributive

complementAndLaws, complementOrLaws :: Boolean a => [Law a]
complementAndLaws = map fromAndLaw dualComplement
complementOrLaws  = map fromOrLaw  dualComplement

absorptionAndLaws, absorptionOrLaws :: Boolean a => [Law a]
absorptionAndLaws = map fromAndLaw dualAbsorption
absorptionOrLaws  = map fromOrLaw  dualAbsorption

deMorganAnd, deMorganOr :: Boolean a => Law a
deMorganAnd = fromAndLaw deMorgan
deMorganOr  = fromOrLaw  deMorgan

doubleComplement :: Boolean a => Law a
doubleComplement = law "double-complement" $ \a ->
   complement (complement a) :==: a

complementTrue, complementFalse :: Boolean a => Law a
complementTrue  = fromAndLaw dualTrueFalse
complementFalse = fromOrLaw  dualTrueFalse

booleanLaws :: Boolean a => [Law a]
booleanLaws =
   map fromAndLaw (idempotent : zeroLaws ++ commutativeMonoidLaws) ++
   map fromOrLaw  (idempotent : zeroLaws ++ commutativeMonoidLaws) ++
   andOverOrLaws ++ orOverAndLaws ++ complementAndLaws ++ complementOrLaws ++
   absorptionAndLaws ++ absorptionOrLaws ++
   [deMorganAnd, deMorganOr, doubleComplement, complementTrue, complementFalse]

--------------------------------------------------------
-- Dual laws

dualDistributive :: DualMonoid a => [Law a]
dualDistributive =
   [leftDistributiveFor (<>) (><), rightDistributiveFor (<>) (><)]

dualAbsorption :: DualMonoid a => [Law a]
dualAbsorption =
   [ law "absorption" $ \a b -> a `f` (a `g` b) :==: a
   | f <- [(<>), flip (<>)]
   , g <- [(><), flip (><)]
   ]

dualComplement :: DualMonoid a => [Law a]
dualComplement =
   [ law "complement" $ \a -> dualCompl a <> a :==: mzero
   , law "complement" $ \a -> a <> dualCompl a :==: mzero
   ]

dualTrueFalse :: DualMonoid a => Law a
dualTrueFalse = law "true-false" $ dualCompl mempty :==: mzero

deMorgan :: DualMonoid a => Law a
deMorgan = law "demorgan" $ \a b ->
   dualCompl (a <> b) :==: dualCompl a >< dualCompl b

--------------------------------------------------------
-- And laws

fromAndLaw :: Law (And a) -> Law a
fromAndLaw = mapLaw And fromAnd

fromOrLaw :: Law (Or a) -> Law a
fromOrLaw = mapLaw Or fromOr

--------------------------------------------------------
-- Tests for Bool instance

propsBoolean :: [Property]
propsBoolean = map property (booleanLaws :: [Law Bool])