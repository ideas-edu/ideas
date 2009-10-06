-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Exercises 
   ( gramSchmidtExercise, linearSystemExercise
   , gaussianElimExercise, systemWithMatrixExercise
   -- , arbSolution
   ) where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Transformation
import Control.Monad
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.Parser
import Domain.LinearAlgebra.Strategies
import Domain.LinearAlgebra.Vector
import Domain.Math.Data.Equation
import Domain.Math.Expr
import Domain.Math.Simplification
import Test.QuickCheck
import Text.Parsing (SyntaxError(..))

gramSchmidtExercise :: Exercise (VectorSpace (Simplified Expr))
gramSchmidtExercise = testableExercise
   { description    = "Gram-Schmidt"
   , exerciseCode   = makeCode "linalg" "gramschmidt"
   , status         = Provisional
   , parser         = \s -> case parseVectorSpace s of
                               (a, [])  -> Right (fmap simplified a)
                               (_, m:_) -> Left $ ErrorMessage $ show m
   , prettyPrinter  = unlines . map show . vectors
   , equivalence    = \x y -> let f = length . filter (not . isZero) . vectors . gramSchmidt
                              in f x == f y
   , extraRules     = rulesGramSchmidt
   , isReady        = orthonormalList . filter (not . isZero) . vectors
   , strategy       = gramSchmidtStrategy
   , randomExercise = simpleGenerator arbitrary
   }

linearSystemExercise :: Exercise (Equations Expr)
linearSystemExercise = testableExercise
   { description    = "Solve Linear System"
   , exerciseCode   = makeCode "linalg" "linsystem"
   , status         = Stable
   , parser         = \s -> case parseSystem s of
                               (a, [])  -> Right (simplify a)
                               (_, m:_) -> Left $ ErrorMessage $ show m
   , prettyPrinter  = unlines . map show
   , equivalence    = \x y -> let f = getSolution . equations . applyD linearSystemStrategy 
                                    . inContext . map toStandardForm
                              in f x == f y
   , extraRules     = equationsRules
   , isReady        = inSolvedForm
   , strategy       = linearSystemStrategy
   , randomExercise = simpleGenerator (fmap matrixToSystem arbMatrix)
   }
   
gaussianElimExercise :: Exercise (Matrix Expr)
gaussianElimExercise = testableExercise
   { description    = "Gaussian Elimination"
   , exerciseCode   = makeCode "linalg" "gaussianelim"
   , status         = Stable
   , parser         = \s -> case parseMatrix s of
                               (a, [])  -> Right (simplify a)
                               (_, m:_) -> Left $ ErrorMessage $ show m
   , prettyPrinter  = ppMatrixWith show
   , equivalence    = \x y -> fmap simplified x === fmap simplified y
   , extraRules     = matrixRules
   , isReady        = inRowReducedEchelonForm
   , strategy       = gaussianElimStrategy
   , randomExercise = simpleGenerator arbMatrix
   }
 
systemWithMatrixExercise :: Exercise (Either (LinearSystem Expr) (Matrix Expr))
systemWithMatrixExercise = testableExercise
   { description    = "Solve Linear System with Matrix"
   , exerciseCode   = makeCode "linalg" "systemwithmatrix"
   , status         = Provisional
   , parser         = \s -> case (parser linearSystemExercise s, parser gaussianElimExercise s) of
                               (Right ok, _) -> Right $ Left  ok
                               (_, Right ok) -> Right $ Right ok
                               (Left _, Left _) -> Left $ ErrorMessage "Syntax error" -- FIX THIS
   , prettyPrinter  = either (unlines . map show) ppMatrix
   , equivalence    = \x y -> let f = either id matrixToSystem
                              in equivalence linearSystemExercise (f x) (f y)
   , extraRules     = map liftRuleContextLeft equationsRules ++ map liftRuleContextRight matrixRules
   , isReady        = either inSolvedForm (const False)
   , strategy       = systemWithMatrixStrategy
   , randomExercise = simpleGenerator (fmap (Left . matrixToSystem) arbMatrix)
   , testGenerator  = fmap (liftM Left) (testGenerator linearSystemExercise)
   }
 
--------------------------------------------------------------
-- Other stuff (to be cleaned up)
                  
instance Arbitrary a => Arbitrary (Vector a) where
   arbitrary   = liftM fromList $ oneof $ map vector [0..2]
   coarbitrary = coarbitrary . toList

instance Arbitrary a => Arbitrary (VectorSpace a) where
   arbitrary = do
      i <- choose (0, 3) -- too many vectors "disables" prime factorization
      j <- choose (0, 10 `div` i)
      xs <- replicateM i (liftM fromList $ replicateM j arbitrary)
      return $ makeVectorSpace xs
   coarbitrary = coarbitrary . vectors

arbMatrix :: Num a => Gen (Matrix a)
arbMatrix = fmap (fmap fromInteger) arbNiceMatrix

liftRuleContextLeft :: Rule (Context a) -> Rule (Context (Either a b))
liftRuleContextLeft = lift $ makeLiftPair (maybeInContext . fmap isLeft) (\a _ -> fmap Left a)

liftRuleContextRight :: Rule (Context b) -> Rule (Context (Either a b))
liftRuleContextRight = lift $ makeLiftPair (maybeInContext . fmap isRight) (\b _ -> fmap Right b)

instance Arbitrary a => Arbitrary (Matrix a) where
   arbitrary = do
      (i, j) <- arbitrary
      arbSizedMatrix (i `mod` 5, j `mod` 5)
   coarbitrary = coarbitrary . rows
   
arbSizedMatrix :: Arbitrary a => (Int, Int) -> Gen (Matrix a)
arbSizedMatrix (i, j) = 
   do rows <- replicateM i (vector j)
      return (makeMatrix rows)

arbUpperMatrix :: (Enum a, Num a) => Gen (Matrix a)
arbUpperMatrix = do
   a <- oneof $ map return [-5 .. 5]
   b <- oneof $ map return [-5 .. 5]
   c <- oneof $ map return [-5 .. 5]
   return $ makeMatrix [[1, a, b], [0, 1, c], [0, 0, 1]]

arbAugmentedMatrix :: (Enum a, Num a) => Gen (Matrix a)
arbAugmentedMatrix = do
   a <- oneof $ map return [-5 .. 5]
   b <- oneof $ map return [-5 .. 5]
   c <- oneof $ map return [-5 .. 5]
   return $ makeMatrix [[1, 0, 0, 1], [a, 1, 0, 1], [b, c, 1, 1]]
   
arbNiceMatrix :: (Enum a, Num a) => Gen (Matrix a)
arbNiceMatrix = do
   m1 <- arbUpperMatrix
   m2 <- arbAugmentedMatrix
   return (multiply m1 m2)