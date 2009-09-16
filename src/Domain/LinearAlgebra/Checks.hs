-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Checks (checks) where

import Domain.Math.Numeric.Views
-- import Common.View
import Domain.Math.Simplification (simplify)
import Domain.LinearAlgebra hiding (getSolution)
import Test.QuickCheck
import Control.Monad
import Common.Utils
import Data.List
import Domain.Math.Expr
import Common.Apply
import Common.Context

-----------------------------------------------------------
--- QuickCheck properties

checks :: IO ()
checks = do
   putStrLn "** Linear algebra"
   thoroughCheck propEchelon
   thoroughCheck propReducedEchelon
   thoroughCheck propSound
   thoroughCheck propSolution

propEchelon :: Matrix Rational -> Bool
propEchelon =
   inRowEchelonForm . matrix . applyD forwardPass . inContext . fmap fromRational

propReducedEchelon :: Matrix Rational -> Bool
propReducedEchelon = 
   inRowReducedEchelonForm . matrix . applyD toReducedEchelon . inContext . fmap fromRational
   
propSound :: Matrix Rational -> Bool
propSound m =
   (matrix . applyD toReducedEchelon . inContext . fmap fromRational) m
   == fmap fromRational (reduce m)

propSolution :: Matrix Rational -> Property
propSolution m1 =
   forAll (arbSolution m1) $ \(solution, m2) -> 
      let m3  = (matrix . applyD toReducedEchelon . inContext . fmap fromRational) m2
          p r = simplify (sum (zipWith g (solution ++ [-1]) r)) == 0
          g r e = fromRational r * e
      in all p (rows m3)