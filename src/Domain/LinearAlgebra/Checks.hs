{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
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
import Common.Strategy

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
   
simplifyMatrix :: (Ord a, Num a) => [a] -> Matrix a -> Gen (Matrix a)
simplifyMatrix solution m = do
   rs <- mapM simplifyRow (rows m)
   return (makeMatrix rs)
 where
   make xs  = xs ++ [sum $ zipWith (*) solution xs]
   f []     = []
   f (x:xs) = map (:xs) (g x) ++ map (x:) (f xs)
   g x      = filter (/=0) [x-1, x+1, negate x]
   simplifyRow r
      | x > 5 = 
           case filter ((< x) . abs . last) $ map make $ f xs of
              []   -> return r
              list -> oneof (map return list) >>= simplifyRow
      | otherwise = 
           return r
    where 
       x  = abs (last r)
       xs = init r
   
-----------------------
-- Assignments: temporarily

reduceMatrixAssignment :: Assignment (MatrixInContext Rational)
reduceMatrixAssignment = makeAssignment
   { shortTitle    = "Reduce to Echelon form"
   , parser        = parseMatrix
   , prettyPrinter = ppRationalMatrix . matrix
   , equivalence   = \x y -> applyD toReducedEchelon (inContext $ matrix x) == applyD toReducedEchelon (inContext $ matrix y)
   , ruleset       = matrixRules
   , finalProperty = inRowReducedEchelonForm . matrix
   , generator     = do m1        <- arbSizedMatrix (3, 3)
                        (sol, m2) <- arbSolution m1
                        m3        <- simplifyMatrix sol m2
                        return $ inContext $ fmap fromSmallInt m3
   , strategy      = toReducedEchelon
   }

instance RealFrac a => Arbitrary (MatrixInContext a) where
   arbitrary = liftM (inContext . fmap fromInteger) (arbitrary)
   coarbitrary mic = coarbitrary (fmap round $ matrix mic :: Matrix Integer)
   
newtype SmallInt = SmallInt Int
   deriving (Show, Eq, Ord, Num)

fromSmallInt :: Num a => SmallInt -> a
fromSmallInt (SmallInt n) = fromIntegral n

instance Arbitrary SmallInt where
   arbitrary = oneof $ map (return . SmallInt) [-15 .. 15]
   coarbitrary (SmallInt n) = coarbitrary n