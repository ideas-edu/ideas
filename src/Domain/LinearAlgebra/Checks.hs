-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Checks (checks, defaultMatrix) where

import Domain.LinearAlgebra hiding (getSolution)
import Test.QuickCheck
import Control.Monad
import Common.Transformation
import Common.Utils
import Data.List
import Common.Context
import Common.Assignment
import Common.Strategy hiding (not)


(t1, p1):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p0 t0 toReducedEchelon
(t2, p2):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p1 t1 toReducedEchelon
(t3, p3):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p2 t2 toReducedEchelon
(t4, p4):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p3 t3 toReducedEchelon
(t5, p5):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p4 t4 toReducedEchelon
(t6, p6):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p5 t5 toReducedEchelon

--t0 = inContext $ Not (Var "x" :||: Var "y") 
t0 = inContext $ makeMatrix [[6,3],[2,4]]
p0 = emptyPrefix 

-----------------------------------------------------------
--- QuickCheck properties

checks :: IO ()
checks = do
   thoroughCheck propEchelon
   thoroughCheck propReducedEchelon
   thoroughCheck propSolution

propEchelon :: Matrix Int -> Bool
propEchelon =
   inRowEchelonForm . matrix . applyD forwardPass . inContext . fmap toRational

propReducedEchelon :: Matrix Int -> Bool
propReducedEchelon = 
   inRowReducedEchelonForm . matrix . applyD toReducedEchelon . inContext . fmap toRational

propSolution :: Matrix Int -> Property
propSolution initial =
   forAll (arbSolution initial) $ \(solution, m) -> 
      let final = matrix $ applyD toReducedEchelon $ inContext $ fmap toRational m
          check n = maybe True ((==n) . round)
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
     