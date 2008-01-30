{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
module Domain.LinearAlgebra.Assignments where

import Common.Transformation
import Common.Assignment
import Domain.LinearAlgebra.Strategies
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.Context
import Domain.LinearAlgebra.Parser
import Test.QuickCheck
import Control.Monad

reduceMatrixAssignment :: Assignment (MatrixInContext Rational)
reduceMatrixAssignment = makeAssignment
   { shortTitle    = "Gaussian Elimination"
   , parser        = parseMatrix
   , prettyPrinter = ppRationalMatrix . matrix
   , equivalence   = \x y -> let f = applyD toReducedEchelon . inContext . matrix
                             in f x == f y
   , ruleset       = matrixRules
   , finalProperty = inRowReducedEchelonForm . matrix
   , generator     = do m1        <- arbSizedMatrix (3, 3)
                        (sol, m2) <- arbSolution m1
                        m3        <- simplifyMatrix sol m2
                        return $ inContext $ fmap fromSmallInt m3
   , strategy      = toReducedEchelon
   }

opgave6b :: Assignment (MatrixInContext Rational)
opgave6b = reduceMatrixAssignment
   { shortTitle = "Opgave 9.6 (b)"
   , generator  = return (inContext ex6b)
   }

ex6b = makeMatrix [[0,1,1,1], [1,2,3,2],[3,1,1,3]]
  
--------------------------------------------------------------
-- Other stuff (to be cleaned up)

instance Arbitrary a => Arbitrary (Matrix a) where
   arbitrary = do
      (i, j) <- arbitrary
      arbSizedMatrix (i `mod` 15, j `mod` 15)
   coarbitrary = coarbitrary . rows

instance RealFrac a => Arbitrary (MatrixInContext a) where
   arbitrary = liftM (inContext . fmap fromInteger) (arbitrary)
   coarbitrary mic = coarbitrary (fmap round $ matrix mic :: Matrix Integer)
   
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
         
---------------------------------------------------------------
-- Small Ints
   
newtype SmallInt = SmallInt Int
   deriving (Show, Eq, Ord, Num)

fromSmallInt :: Num a => SmallInt -> a
fromSmallInt (SmallInt n) = fromIntegral n

instance Arbitrary SmallInt where
   arbitrary = oneof $ map (return . SmallInt) [-15 .. 15]
   coarbitrary (SmallInt n) = coarbitrary n