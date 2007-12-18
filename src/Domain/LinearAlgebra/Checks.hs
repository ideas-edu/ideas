-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Checks (checks, defaultMatrix, reduceMatrixAssignment) where

import Domain.LinearAlgebra
import Test.QuickCheck
import Control.Monad
import Common.Transformation
import Common.Utils
import Data.List
import Common.Assignment
import Debug.Trace

-----------------------------------------------------------
--- QuickCheck properties

checks :: IO ()
checks = do
   thoroughCheck propEchelon
   thoroughCheck propReducedEchelon
   thoroughCheck propSolution

propEchelon :: Matrix Int -> Bool
propEchelon =
   inRowEchelonForm . matrix . applyD toEchelon . inContext . fmap toRational

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
     
-----------------------------------------------------------
--- QuickCheck generators
   
instance Arbitrary a => Arbitrary (Matrix a) where
   arbitrary = do
      (i, j) <- arbitrary
      arbSizedMatrix (i `mod` 15, j `mod` 15)
   coarbitrary = coarbitrary . rows
   
arbSizedMatrix :: Arbitrary a => (Int, Int) -> Gen (Matrix a)
arbSizedMatrix (i, j) = 
   do rows <- replicateM i (vector j)
      return (makeMatrix rows)

arbSolution :: (Arbitrary a, Num a) => Matrix a -> Gen ([a], Matrix a)
arbSolution m = do
   solution <- vector (snd $ dimensions m)
   let finalCol  = map (return . sum . zipWith (*) solution) (rows m)
       newMatrix = makeMatrix $ zipWith (++) (rows m) finalCol
   return (solution, newMatrix)
   
-----------------------
-- Assignments: temporarily

filterGen :: (a -> Bool) -> Gen a -> Gen a
filterGen p gen = do
   a <- gen
   if p a then return a else filterGen p gen

q = quickCheck $ forAll (generator reduceMatrixAssignment) $ \m -> trace (show m) True
-- checkAssignment reduceMatrixAssignment

reduceMatrixAssignment :: Assignment (MatrixInContext Rational)
reduceMatrixAssignment = makeAssignment
   { shortTitle    = "Reduce to Echelon form"
   , parser        = parseMatrix
   , prettyPrinter = ppRationalMatrix . matrix
   , equivalence   = \x y -> applyD toReducedEchelon (inContext $ matrix x) == applyD toReducedEchelon (inContext $ matrix y)
   , ruleset       = matrixRules
   , finalProperty = inRowReducedEchelonForm . matrix
   , generator     = let rec _ = do m <- liftM (inContext . fmap fromInteger) $ sized $ \_ -> arbSizedMatrix (4, 3)
                                    if finalProperty reduceMatrixAssignment m then 
                                          trace ("REC" ++ show m) $ rec () else return m
                     in liftM (inContext . fmap fromInteger) $
                        {- filterGen (not . inRowReducedEchelonForm)-} (arbSizedMatrix (4, 3))
   , strategy      = toReducedEchelon
   }

instance RealFrac a => Arbitrary (MatrixInContext a) where
   arbitrary = liftM (inContext . fmap fromInteger) (arbitrary)
   coarbitrary mic = coarbitrary (fmap round $ matrix mic :: Matrix Integer)