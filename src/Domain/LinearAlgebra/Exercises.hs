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
module Domain.LinearAlgebra.Exercises where

import Common.Apply
import Common.Transformation
import Common.Exercise
import Common.Context
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
import Data.Ratio

import Domain.Math.SExpr
import Domain.Math.Parser

solveGramSchmidt :: Exercise (Context [Vector SExpr])
solveGramSchmidt = makeExercise
   { shortTitle    = "Gram-Schmidt"
   , parser        = \s -> case parseVectors s of
                              (a, [])  -> Right a
                              (_, msg) -> Left $ text $ show msg
   , prettyPrinter = unlines . map show . fromContext
   , equivalence   = \x y -> let f = fromContext . applyD gramSchmidt
                             in f x == f y
   , ruleset       = rulesGramSchmidt
   , finalProperty = orthonormalList . filter ((/=0) . norm) . fromContext
   , strategy      = gramSchmidt
   , generator     = liftM inContext arbBasis 
   }

solveSystemExercise :: Exercise (EqsInContext SExpr)
solveSystemExercise = makeExercise
   { shortTitle    = "Solve Linear System"
   , parser        = \s -> case parseSystem s of
                              (a, [])  -> Right a
                              (_, msg) -> Left $ text $ show msg
   , prettyPrinter = unlines . map show . equations
   , equivalence   = \x y -> let f = getSolution . equations . applyD generalSolutionLinearSystem 
                                   . inContext . map toStandardForm . equations
                             in f x == f y
   , ruleset       = equationsRules
   , finalProperty = inSolvedForm . equations
   , strategy      = generalSolutionLinearSystem
   , generator     = do m <- generator reduceMatrixExercise
                        return $ fmap matrixToSystem m
   }
   
reduceMatrixExercise :: Exercise (MatrixInContext SExpr)
reduceMatrixExercise = makeExercise
   { shortTitle    = "Gaussian Elimination"
   , parser        = \s -> case parseMatrix s of
                              (a, [])  -> Right a
                              (_, msg) -> Left $ text $ unlines msg
   , prettyPrinter = ppMatrix . matrix
   , equivalence   = \x y -> fromContext x === fromContext y
   , ruleset       = matrixRules
   , finalProperty = inRowReducedEchelonForm . matrix
   , generator     = fmap (inContext . fmap fromInteger) arbNiceMatrix
   , strategy      = toReducedEchelon
   }

solveSystemWithMatrixExercise :: Exercise (Context (Either (LinearSystem SExpr) (Matrix SExpr)))
solveSystemWithMatrixExercise = makeExercise
   { shortTitle    = "Solve Linear System with Matrix"
   , parser        = \s -> case (parser solveSystemExercise s, parser reduceMatrixExercise s) of
                              (Right ok, _) -> Right (fmap Left ok)
                              (_, Right ok) -> Right (fmap Right ok)
                              (Left doc1, Left doc2) -> Left (text "Error") -- FIX THIS
   , prettyPrinter = either (unlines . map show . id) ppMatrix . fromContext
   , equivalence   = \x y -> let f = applyD toReducedEchelon . inContext
                                 g = f . either (fst . systemToMatrix) id . fromContext
                             in g x == g y
   , ruleset       = map liftRuleContextLeft equationsRules ++ map liftRuleContextRight matrixRules
   , finalProperty = either inSolvedForm (const False) . fromContext
   , strategy      = generalSolutionSystemWithMatrix
   , generator     = liftM (fmap Left) (generator solveSystemExercise)
   }

opgave6b :: Exercise (MatrixInContext SExpr)
opgave6b = reduceMatrixExercise
   { shortTitle = "Opgave 9.6 (b)"
   , generator  = return $ inContext $ makeMatrix [[0,1,1,1], [1,2,3,2],[3,1,1,3]]
   }
  
--------------------------------------------------------------
-- Other stuff (to be cleaned up)

instance Argument SExpr where
   makeArgDescr = argDescrSExpr

argDescrSExpr :: String -> ArgDescr SExpr
argDescrSExpr descr = ArgDescr descr Nothing parseRatio show
 where
   parseRatio = either (const Nothing) (Just . simplifyExpr) . parseExpr
                  
instance Arbitrary a => Arbitrary (Vector a) where
   arbitrary   = liftM fromList arbitrary
   coarbitrary = coarbitrary . toList

{- instance Arbitrary MySqrt where
   arbitrary = oneof $ map (return . fromInteger) [-10 .. 10]
   coarbitrary = coarbitrary . fromMySqrt -}

arbBasis :: Gen [Vector SExpr]
arbBasis = do
   --i <- oneof $ map return [0..5]
   --j <- oneof $ map return [0..5]
   replicateM 2 $ liftM fromList $ replicateM 2 $ liftM fromInteger arbitrary

liftRuleLeft :: Rule a -> Rule (Either a b)
liftRuleLeft = lift $ makeLiftPair isLeft (\a _ -> Left a)

liftRuleRight :: Rule b -> Rule (Either a b)
liftRuleRight = lift $ makeLiftPair isRight (\b _ -> Right b)

liftRuleContextLeft :: Rule (Context a) -> Rule (Context (Either a b))
liftRuleContextLeft = lift $ makeLiftPair (maybeInContext . fmap isLeft) (\a _ -> fmap Left a)

liftRuleContextRight :: Rule (Context b) -> Rule (Context (Either a b))
liftRuleContextRight = lift $ makeLiftPair (maybeInContext . fmap isRight) (\b _ -> fmap Right b)

instance Arbitrary a => Arbitrary (Matrix a) where
   arbitrary = do
      (i, j) <- arbitrary
      arbSizedMatrix (i `mod` 15, j `mod` 15)
   coarbitrary = coarbitrary . rows

{- instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
   arbitrary = liftM fromInteger arbitrary
   coarbitrary r = coarbitrary (numerator r) . coarbitrary (denominator r) -}
   
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

       
---------------------------------------------------------------
-- Small Ints
   {-
newtype SmallInt = SmallInt Int
   deriving (Show, Eq, Ord, Num)

fromSmallInt :: Num a => SmallInt -> a
fromSmallInt (SmallInt n) = fromIntegral n

instance Arbitrary SmallInt where
   arbitrary = oneof $ map (return . SmallInt) [-15 .. 15]
   coarbitrary (SmallInt n) = coarbitrary n
   
newtype ShowRational = ShowRational Rational
   deriving (Eq, Num, Fractional)

instance Show ShowRational where
   show (ShowRational r) = ppRational r -}