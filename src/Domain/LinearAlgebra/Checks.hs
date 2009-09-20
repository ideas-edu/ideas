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

import Domain.Math.Simplification (simplify)
import Domain.LinearAlgebra hiding (getSolution)
import Test.QuickCheck
import Control.Monad
import Common.Utils
import Data.List
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
   inRowReducedEchelonForm . matrix . applyD gaussianElimStrategy . inContext . fmap fromRational
   
propSound :: Matrix Rational -> Bool
propSound m =
   (matrix . applyD gaussianElimStrategy . inContext . fmap fromRational) m
   == fmap fromRational (reduce m)

propSolution :: Matrix Rational -> Property
propSolution m1 =
   forAll (arbSolution m1) $ \(solution, m2) -> 
      let m3  = (matrix . applyD gaussianElimStrategy . inContext . fmap fromRational) m2
          p r = simplify (sum (zipWith g (solution ++ [-1]) r)) == 0
          g   = (*) . fromRational
      in all p (rows m3)

arbSolution :: (Arbitrary a, Num a) => Matrix a -> Gen ([a], Matrix a)
arbSolution m = do
   solution <- vector (snd $ dimensions m)
   let finalCol  = map (return . sum . zipWith (*) solution) (rows m)
       newMatrix = makeMatrix $ zipWith (++) (rows m) finalCol
   return (solution, newMatrix)