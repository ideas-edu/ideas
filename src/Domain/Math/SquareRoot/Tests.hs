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
module Domain.Math.SquareRoot.Tests (tests) where

import Common.Algebra.Field
import Common.Algebra.Law
import Common.TestSuite
import Domain.Math.Data.SquareRoot

-------------------------------------------------------------------
-- Testing
 
tests :: TestSuite
tests = mapM_ f (commutativeRingLaws ++ subtractionLaws)
 where
   f :: Law (SafeNum (SquareRoot Rational)) -> TestSuite
   f p = addProperty (show p) p
      
-- Todo: move these to Common.Algebra
subtractionLaws :: Num a => [Law a]
subtractionLaws = 
   [ law  "1" $ \a b   -> a - b :==: a + (-b)
   , law  "2" $ \a     -> a - a :==: 0
   , law  "3" $ \a     -> a - 0 :==: a
   , law  "4" $ \a     -> 0 - a :==: -a
   , law  "5" $ \a b c -> a - (b + c) :==: (a - b) - c
   , law  "6" $ \a b c -> a - (b - c) :==: (a - b) + c
   , law  "7" $ \a b c -> a + (b - c) :==: (a + b) - c
   , law  "8" $ \a b   -> a - (-b) :==: a + b
   , law  "9" $ \a b   -> -(a - b) :==: -a + b
   , law "10" $ \a b c -> a * (b - c) :==: (a * b) - (a * c)
   , law "11" $ \a b c -> (a - b) * c :==: (a * c) - (b * c)
   ]