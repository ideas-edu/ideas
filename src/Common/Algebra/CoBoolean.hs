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
module Common.Algebra.CoBoolean
   ( CoBoolean(..)
   , conjunctions, disjunctions
   , (.||.), (.&&.)
   ) where

import Common.Algebra.Boolean
import Common.Algebra.CoGroup
import Common.Algebra.Group
import Common.Algebra.SmartGroup
import Control.Arrow
import Data.Maybe

class BoolValue a => CoBoolean a where
   isAnd        :: a -> Maybe (a, a)
   isOr         :: a -> Maybe (a, a)
   isComplement :: a -> Maybe a

instance CoBoolean a => CoMonoid (And a) where
   isEmpty  = isTrue . fromAnd
   isAppend = fmap (And *** And) . isAnd . fromAnd

instance CoBoolean a => CoMonoidZero (And a) where
   isMonoidZero = isFalse . fromAnd

instance CoBoolean a => CoMonoid (Or a) where
   isEmpty  = isFalse . fromOr
   isAppend = fmap (Or *** Or) . isOr . fromOr

instance CoBoolean a => CoMonoidZero (Or a) where
   isMonoidZero = isTrue . fromOr

conjunctions :: CoBoolean a => a -> [a]
conjunctions = map fromAnd . associativeList . And

disjunctions :: CoBoolean a => a -> [a]
disjunctions = map fromOr . associativeList . Or

instance BoolValue a => BoolValue (Smart a) where
   fromBool = Smart   . fromBool
   isTrue   = isTrue  . fromSmart
   isFalse  = isFalse . fromSmart

instance (Boolean a, CoBoolean a) => Boolean (Smart a) where
   a <&&> b = fmap fromAnd $ fromSmartZero $
      SmartZero (fmap And a) <> SmartZero (fmap And b)
   a <||> b = fmap fromOr $ fromSmartZero $
      SmartZero (fmap Or a) <> SmartZero (fmap Or b)
   complement (Smart a)
      | isTrue  a = false
      | isFalse a = true
      | otherwise = Smart $ fromMaybe (complement a) (isComplement a)

infixr 4 .||.
infixr 5 .&&.

(.&&.), (.||.) :: (Boolean a, CoBoolean a) => a -> a -> a
a .&&. b = fromSmart $ Smart a <&&> Smart b
a .||. b = fromSmart $ Smart a <||> Smart b