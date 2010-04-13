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
module Domain.LinearAlgebra.Checks (checks) where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Utils
import Control.Monad
import Data.List
import Domain.LinearAlgebra hiding (getSolution)
import Domain.Math.Expr
import Domain.Math.Simplification (simplify)
import Test.QuickCheck

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
   withoutContext inRowEchelonForm . applyD forwardPass . gaussContext

propReducedEchelon :: Matrix Rational -> Bool
propReducedEchelon = 
   withoutContext inRowReducedEchelonForm . applyD gaussianElimStrategy . gaussContext
   
propSound :: Matrix Rational -> Bool
propSound m =
   (fromContext . applyD gaussianElimStrategy . gaussContext) m
   == Just (fmap fromRational (reduce m))

propSolution :: Matrix Rational -> Property
propSolution m1 =
   forAll (arbSolution m1) $ \(solution, m2) -> 
      let m3  = (fromContext . applyD gaussianElimStrategy . gaussContext) m2
          p r = simplify (sum (zipWith g (solution ++ [-1]) r)) == 0
          g   = (*) . fromRational
      in maybe False (all p . rows) m3

arbSolution :: (Arbitrary a, Num a) => Matrix a -> Gen ([a], Matrix a)
arbSolution m = do
   solution <- vector (snd $ dimensions m)
   let finalCol  = map (return . sum . zipWith (*) solution) (rows m)
       newMatrix = makeMatrix $ zipWith (++) (rows m) finalCol
   return (solution, newMatrix)
   
withoutContext :: (a -> Bool) -> Context a -> Bool
withoutContext f = maybe False f . fromContext

gaussContext :: Matrix Rational -> Context (Matrix Expr)
gaussContext = inContext gaussianElimExercise . fmap fromRational