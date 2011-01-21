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
module Domain.Math.Polynomial.Tests (tests) where

import Common.TestSuite
import Common.Algebra.Field
import Common.Algebra.Law
import Domain.Math.Data.Polynomial
import Control.Monad

tests :: TestSuite
tests = suite "Polynomial is a commutative ring" $ 
   forM_ (commutativeRingLaws :: [Law (SafeNum (Polynomial Int))]) $ \p ->
      addProperty (show p) p