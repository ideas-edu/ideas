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
module Domain.LinearAlgebra.Checks (checks, defaultMatrix) where

import Domain.Math.View.Numeric
import Common.View
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
   thoroughCheck propSolution

propEchelon :: Matrix Int -> Bool
propEchelon =
   inRowEchelonForm . matrix . applyD forwardPass . inContext . fmap fromIntegral

propReducedEchelon :: Matrix Int -> Bool
propReducedEchelon = 
   inRowReducedEchelonForm . matrix . applyD toReducedEchelon . inContext . fmap fromIntegral

propSolution :: Matrix Int -> Property
propSolution initial =
   forAll (arbSolution initial) $ \(solution, m) -> 
      let final = matrix $ applyD toReducedEchelon $ inContext $ fmap fromIntegral m
          check :: Int -> Maybe Expr -> Bool
          check n me = maybe False (==n) (join $ fmap (match integralView) me)
      in and $ zipWith check solution (getSolution final)
      
getSolution :: Num a => Matrix a -> [Maybe a]
getSolution m = map (merge . concatMap checkRow . findIndices (==1)) (columns m)
 where
   checkRow r = let xs = row r m
                in [ last xs | filter (/=0) (init xs) == [1] ]
   merge (x:xs) = if all (==x) xs then Just x else Nothing
   merge _ = Nothing

defaultMatrix :: Matrix Rational
defaultMatrix = makeMatrix $ reverse [[4,1,-1,6],[1,2,-1,1], [6,-3,1,12]]
-- x=2, y=1, z=3
     