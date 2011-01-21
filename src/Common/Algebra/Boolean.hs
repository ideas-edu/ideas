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
module Common.Algebra.Boolean 
   ( -- * Boolean algebra
     Boolean(..)
   , andOverOrLaws, orOverAndLaws
   , complementAndLaws, complementOrLaws
   , absorptionAndLaws, absorptionOrLaws
   , deMorganAnd, deMorganOr
   , doubleComplement, complementTrue, complementFalse
   , booleanLaws
     -- * Dual monoid
   , DualMonoid(..)
     -- * And monoid
   , And(..), fromAndLaw
     -- * Or monoid
   , Or(..), fromOrLaw
   ) where

import Common.Algebra.Law
import Common.Algebra.Group
import Test.QuickCheck hiding ((><))
import Control.Applicative

--------------------------------------------------------
-- Boolean algebra

class Boolean a where
   -- and
   (<&&>) :: a -> a -> a
   true   :: a
   -- or
   (<||>) :: a -> a -> a
   false  :: a
   -- complement
   complement :: a -> a

instance Boolean Bool where
   (<&&>)     = (&&)
   (<||>)     = (||)
   true       = True
   false      = False
   complement = not

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
-- Dual monoid for a monoid (and for or, and vice versa)

class MonoidZero a => DualMonoid a where
   (><)      :: a -> a -> a
   dualCompl :: a -> a

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
   [ law "complement" $ \a -> dualCompl a <> a :==: zero
   , law "complement" $ \a -> a <> dualCompl a :==: zero
   ]

dualTrueFalse :: DualMonoid a => Law a
dualTrueFalse = law "true-false" $ dualCompl mempty :==: zero

deMorgan :: DualMonoid a => Law a
deMorgan = law "demorgan" $ \a b -> 
   dualCompl (a <> b) :==: dualCompl a >< dualCompl b
   
--------------------------------------------------------
-- And monoid

newtype And a = And {fromAnd :: a}
   deriving (Show, Eq, Ord, Functor, Arbitrary, CoArbitrary)

instance Applicative And where
   pure            = And
   And f <*> And a = And (f a)

instance Boolean a => Monoid (And a) where
   mempty  = pure true
   mappend = liftA2 (<&&>)

instance Boolean a => MonoidZero (And a) where
   zero = pure false

instance Boolean a => DualMonoid (And a) where
   (><)      = liftA2 (<||>)
   dualCompl = liftA complement

fromAndLaw :: Law (And a) -> Law a
fromAndLaw = mapLaw And fromAnd

--------------------------------------------------------
-- Or monoid

newtype Or a  = Or {fromOr :: a} 
   deriving (Show, Eq, Ord, Functor, Arbitrary, CoArbitrary)

instance Applicative Or where
   pure          = Or
   Or f <*> Or a = Or (f a)

instance Boolean a => Monoid (Or a) where
   mempty  = pure false
   mappend = liftA2 (<||>)
   
instance Boolean a => MonoidZero (Or a) where
   zero = pure true

instance Boolean a => DualMonoid (Or a) where
   (><)      = liftA2 (<&&>)
   dualCompl = liftA complement

fromOrLaw :: Law (Or a) -> Law a
fromOrLaw = mapLaw Or fromOr

main = mapM_ quickCheck (booleanLaws :: [Law Bool])