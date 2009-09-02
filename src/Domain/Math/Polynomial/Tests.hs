module Domain.Math.Polynomial.Tests where

import Control.Monad
import Common.Apply
import Common.Exercise
import Common.Context
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.ExercisesDWO
import Domain.Math.Polynomial.Exercises
import Domain.Math.Polynomial.Generators
import Test.QuickCheck

-- see the derivations for the DWO exercise set
seeLE  n = printDerivation linearExercise $ concat linearEquations !! n
seeQE  n = printDerivation quadraticExercise $ OrList $ return $ concat quadraticEquations !! n
seeHDE n = printDerivation higherDegreeExercise $ OrList $ return $ higherDegreeEquations !! n

-- test strategies with DWO exercise set
testLE  = concat $ zipWith (f linearExercise)       [0..] $ concat linearEquations
testQE  = concat $ zipWith (f quadraticExercise)    [0..] $ map (OrList . return) $ concat quadraticEquations
testHDE = concat $ zipWith (f higherDegreeExercise) [0..] $ map (OrList . return) $ higherDegreeEquations

f s n e = map p (g (applyAll (strategy s) (inContext e))) where
  g xs | null xs   = error $ show n ++ ": " ++ show e
       | otherwise = xs
  p a  | finalProperty s (fromContext a) = n
       | otherwise = error $ show n ++ ": " ++ show e ++ "  =>  " ++ show (fromContext a)
       
randomLE = quickCheck $ forAll (liftM2 (:==:) (sized linearGen) (sized linearGen)) $ \eq -> 
   (>0) (sum (take 10 $ f linearExercise 1 (eq)))
randomQE = quickCheck $ forAll (liftM2 (:==:) (sized quadraticGen) (sized quadraticGen)) $ \eq -> 
   (>0) (sum (take 10 $ f quadraticExercise 1 (OrList [eq])))