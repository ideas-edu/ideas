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
module Common.Algebra.CoBoolean 
   ( CoBoolean(..)
   , conjunctions, disjunctions
   , (.||.), (.&&.)
   ) where

import Common.Algebra.Boolean
import Common.Algebra.CoGroup
import Common.Algebra.Group
import Control.Arrow
import Control.Applicative

class BoolValue a => CoBoolean a where
   isAnd        :: a -> Maybe (a, a)
   isOr         :: a -> Maybe (a, a)
   isComplement :: a -> Maybe a
   
instance CoBoolean a => CoMonoid (And a) where
   isEmpty  = isTrue . fromAnd
   isAppend = fmap (And *** And) . isAnd . fromAnd

instance CoBoolean a => CoMonoidZero (And a) where
   isZero = isFalse . fromAnd
   
instance CoBoolean a => CoMonoid (Or a) where
   isEmpty  = isFalse . fromOr
   isAppend = fmap (Or *** Or) . isOr . fromOr

instance CoBoolean a => CoMonoidZero (Or a) where
   isZero = isTrue . fromOr
   
conjunctions :: CoBoolean a => a -> [a]
conjunctions = map fromAnd . associativeList . And

disjunctions :: CoBoolean a => a -> [a]
disjunctions = map fromOr . associativeList . Or

infixr 4 .||. 
infixr 5 .&&.

(.&&.), (.||.) :: (Boolean a, CoBoolean a) => a -> a -> a
a .&&. b = fromAnd $ smartZero (And a) (And b)
a .||. b = fromOr  $ smartZero (Or a)  (Or b)

smartZero :: (CoMonoidZero a, MonoidZero a) => a -> a -> a
smartZero a b 
   | isEmpty a = b
   | isZero  a = zero
   | isEmpty b = a
   | isZero  b = zero
   | otherwise = a <> b