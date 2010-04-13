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

import Control.Monad
import Test.QuickCheck
import Domain.Math.Data.SquareRoot
import Domain.Math.Numeric.Laws
import Common.Utils ()

-------------------------------------------------------------------
-- Testing
 
tests :: IO ()
tests = 
   testNumLaws  "square roots" squareRootGen
   -- 	testFracLaws "square roots" squareRootGen

squareRootGen :: Gen (SquareRoot Rational)
squareRootGen = do
   n <- choose (0, 10)
   let f r1 r2 = fromRational r1 * sqrtRational (abs r2)
   ps <- replicateM n $ liftM2 f arbitrary arbitrary
   return (sum ps)