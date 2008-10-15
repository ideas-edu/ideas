{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
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
module Domain.LinearAlgebra.Exercises 
   ( solveGramSchmidt, solveSystemExercise, reduceMatrixExercise
   , solveSystemWithMatrixExercise, opgave6b, opgaveVarMatrix, opgaveVarMatrix2
   , arbSolution
   ) where

import Common.Apply
import Common.Transformation
import Common.Exercise
import Common.Context
import Common.Parsing (SyntaxError(..))
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.Strategies
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.Parser
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Vector
import Test.QuickCheck
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Symbolic
import Domain.Math.SExpr
import Domain.Math.Parser

laDomain :: String
laDomain = "linalg"
 
solveGramSchmidt :: Exercise [Vector SExprGS]
solveGramSchmidt = makeExercise
   { identifier    = "Gram-Schmidt" -- TODO: simplify code
   , domain        = laDomain
   , description   = "Gram-Schmidt"
   , status        = Stable
   , parser        = \s -> case parseVectors s of
                              (a, [])  -> Right a
                              (_, m:_) -> Left $ ErrorMessage $ show m
   , prettyPrinter = unlines . map show
   , equivalence   = \x y -> let f = fromContext . applyD gramSchmidt . inContext
                             in f x == f y
   , ruleset       = rulesGramSchmidt
   , finalProperty = orthonormalList . filter ((/=0) . norm)
   , strategy      = gramSchmidt
   , generator     = arbBasis 
   }

solveSystemExercise :: Exercise (Equations SExprLin)
solveSystemExercise = makeExercise
   { identifier    = "Solve Linear System" -- TODO: simplify code
   , domain        = laDomain
   , description   = "Solve Linear System"
   , status        = Stable
   , parser        = \s -> case parseSystem s of
                              (a, [])  -> Right a
                              (_, m:_) -> Left $ ErrorMessage $ show m
   , prettyPrinter = unlines . map show
   , equivalence   = \x y -> let f = getSolution . equations . applyD generalSolutionLinearSystem 
                                   . inContext . map toStandardForm
                             in f x == f y
   , ruleset       = equationsRules
   , finalProperty = inSolvedForm
   , strategy      = generalSolutionLinearSystem
   , generator     = do m <- generator reduceMatrixExercise
                        return $ forgetList $ matrixToSystem m
   }
   
reduceMatrixExercise :: Exercise (Matrix SExpr)
reduceMatrixExercise = makeExercise
   { identifier    = "Gaussian Elimination" -- TODO: simplify code
   , domain        = laDomain
   , description   = "Gaussian Elimination"
   , status        = Stable
   , parser        = \s -> case parseMatrix s of
                              (a, [])  -> Right a
                              (_, m:_) -> Left $ ErrorMessage $ show m
   , prettyPrinter = ppMatrixWith (ppExprPrio 0 . toExpr)
   , equivalence   = (===)
   , ruleset       = matrixRules
   , finalProperty = inRowReducedEchelonForm
   , generator     = fmap (fmap fromInteger) arbNiceMatrix
   , strategy      = toReducedEchelon
   }
 
solveSystemWithMatrixExercise :: Exercise (Either (LinearSystem SExpr) (Matrix SExpr))
solveSystemWithMatrixExercise = makeExercise
   { identifier    = "Solve Linear System with Matrix" -- TODO: simplify code
   , domain        = laDomain
   , description   = "Solve Linear System with Matrix"
   , status        = Stable
   , parser        = \s -> case (parser solveSystemExercise s, parser reduceMatrixExercise s) of
                              (Right ok, _) -> Right $ Left $ forgetList ok
                              (_, Right ok) -> Right $ Right ok
                              (Left _, Left _) -> Left $ ErrorMessage "Syntax error" -- FIX THIS
   , prettyPrinter = either (unlines . map show) ppMatrix
   , equivalence   = \x y -> let f = either forgetList (forgetList . matrixToSystem)
                             in equivalence solveSystemExercise (f x) (f y)
   , ruleset       = map liftRuleContextLeft equationsRules ++ map liftRuleContextRight matrixRules
   , finalProperty = either inSolvedForm (const False)
   , strategy      = generalSolutionSystemWithMatrix
   , generator     = liftM (Left . forgetList) (generator solveSystemExercise)
   }

opgave6b :: Exercise (Matrix SExpr)
opgave6b = reduceMatrixExercise
   { identifier = "opg9.6b"
   , generator  = return $ makeMatrix [[0,1,1,1], [1,2,3,2],[3,1,1,3]]
   }

opgaveVarMatrix2 :: Exercise (Matrix SExpr)
opgaveVarMatrix2 = reduceMatrixExercise
   { identifier = "matrix-with-var2"
   , generator  = return $ makeMatrix [[-1,-1,variable "a"],[2,4,2]]
   }

opgaveVarMatrix :: Exercise (Matrix SExpr)
opgaveVarMatrix = reduceMatrixExercise
   { identifier = "matrix-with-var"
   , generator  = return $ makeMatrix [[1,lam,0,1,0,0],[lam,1,lam*lam-1,0,1,0],[0,2,-1,0,0,1]]
   }
 where lam = variable "L"
 
--------------------------------------------------------------
-- Other stuff (to be cleaned up)

forgetList :: Functor f => [f (SExprF a)] -> [f (SExprF b)]
forgetList xs = map (fmap forget) xs

instance Simplification a => Argument (SExprF a) where
   makeArgDescr = argDescrSExpr

argDescrSExpr :: Simplification a => String -> ArgDescr (SExprF a)
argDescrSExpr descr = ArgDescr descr Nothing parseRatio show arbitrary
 where
   parseRatio = either (const Nothing) (Just . simplifyExpr) . parseExpr
                  
instance Arbitrary a => Arbitrary (Vector a) where
   arbitrary   = liftM fromList $ oneof $ map vector [0..2]
   coarbitrary = coarbitrary . toList

{- instance Arbitrary MySqrt where
   arbitrary = oneof $ map (return . fromInteger) [-10 .. 10]
   coarbitrary = coarbitrary . fromMySqrt -}

arbBasis :: Simplification a => Gen [Vector (SExprF a)]
arbBasis = do
   --i <- oneof $ map return [0..5]
   --j <- oneof $ map return [0..5]
   replicateM 2 $ liftM fromList $ replicateM 2 $ liftM fromInteger arbitrary

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

arbSolution :: (Arbitrary a, Num a) => Matrix a -> Gen ([a], Matrix a)
arbSolution m = do
   solution <- vector (snd $ dimensions m)
   let finalCol  = map (return . sum . zipWith (*) solution) (rows m)
       newMatrix = makeMatrix $ zipWith (++) (rows m) finalCol
   return (solution, newMatrix)

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