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

import Common.Algebra.Group
import Common.Algebra.Field
import Common.Algebra.Law
import Common.TestSuite
import Domain.Math.Data.SquareRoot

-------------------------------------------------------------------
-- Testing
 
tests :: TestSuite
tests = mapM_ f $ commutativeRingLaws ++ 
                  distributiveSubtractionLaws ++
                  map fromAdditiveLaw appendInverseLaws
 where
   f :: Law (SafeNum (SquareRoot Rational)) -> TestSuite
   f p = addProperty (show p) p