{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
module Domain.LinearAlgebra.Assignments where

import Common.Transformation
import Common.Assignment
import Common.Context
import Domain.LinearAlgebra.Strategies
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.Parser
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Vector
import Test.QuickCheck
import Control.Monad
import Data.Ratio

solveGramSchmidt :: Assignment (Context [Vector MySqrt])
solveGramSchmidt = makeAssignment
   { shortTitle    = "Gram-Schmidt"
   , prettyPrinter = unlines . map show . fromContext
   , ruleset       = rulesGramSchmidt
   , finalProperty = orthonormalList . filter ((/=0) . norm) . fromContext
   , strategy      = gramSchmidt
   , generator     = liftM inContext arbBasis 
   }

solveSystemAssignment :: Assignment (EqsInContext Rational)
solveSystemAssignment = makeAssignment
   { shortTitle    = "Solve Linear System"
   , parser        = either (\(x,y) -> Left (x, fmap inContext y)) (Right . inContext) . parseSystem
   , prettyPrinter = unlines . map (show . fmap (fmap ShowRational)) . equations
   , equivalence   = \x y -> let f = getSolution . equations . applyD generalSolutionLinearSystem 
                                   . inContext . map toStandardForm . equations
                             in f x == f y
   , ruleset       = equationsRules
   , finalProperty = inSolvedForm . equations
   , strategy      = generalSolutionLinearSystem
   }
   
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

solveSystemWithMatrixAssignment :: Assignment (Context (Either (LinearSystem Rational) (Matrix Rational)))
solveSystemWithMatrixAssignment = makeAssignment
   { shortTitle    = "Solve Linear System with Matrix"
   , parser        = \s -> case (parser solveSystemAssignment s, parser reduceMatrixAssignment s) of
                              (Right ok, _) -> Right (fmap Left ok)
                              (_, Right ok) -> Right (fmap Right ok)
                              (Left (doc1, _), Left (doc2, _)) -> Left (text "Error", Nothing) -- FIX THIS
   , prettyPrinter = either (unlines . map (show . fmap (fmap ShowRational))) ppRationalMatrix . fromContext
   , equivalence   = \x y -> let f = applyD toReducedEchelon . inContext
                                 g = f . either (fst . systemToMatrix) id . fromContext
                             in g x == g y
   , ruleset       = map liftRuleContextLeft equationsRules ++ map liftRuleContextRight matrixRules
   , finalProperty = either inSolvedForm (const False) . fromContext
   , strategy      = generalSolutionSystemWithMatrix
   , generator     = liftM (fmap Left) arbitrary
   }

opgave6b :: Assignment (MatrixInContext Rational)
opgave6b = reduceMatrixAssignment
   { shortTitle = "Opgave 9.6 (b)"
   , generator  = return $ inContext $ makeMatrix [[0,1,1,1], [1,2,3,2],[3,1,1,3]]
   }
  
--------------------------------------------------------------
-- Other stuff (to be cleaned up)

instance Arbitrary a => Arbitrary (Vector a) where
   arbitrary   = liftM fromList arbitrary
   coarbitrary = coarbitrary . toList

instance Arbitrary MySqrt where
   arbitrary = oneof $ map (return . fromInteger) [-10 .. 10]
   coarbitrary = coarbitrary . fromMySqrt

arbBasis :: Gen [Vector MySqrt]
arbBasis = do
   j <- oneof $ map return [0..5]
   i <- oneof $ map return [0..5] -- oneof $ map return [0..j]
   replicateM i (liftM fromList $ replicateM j arbitrary)

liftRuleLeft :: Rule a -> Rule (Either a b)
liftRuleLeft = liftRule $ LiftPair isLeft (\a _ -> Left a)

liftRuleRight :: Rule b -> Rule (Either a b)
liftRuleRight = liftRule $ LiftPair isRight (\b _ -> Right b)

liftRuleContextLeft :: Rule (Context a) -> Rule (Context (Either a b))
liftRuleContextLeft = liftRule $ LiftPair (maybeInContext . fmap isLeft) (\a _ -> fmap Left a)

liftRuleContextRight :: Rule (Context b) -> Rule (Context (Either a b))
liftRuleContextRight = liftRule $ LiftPair (maybeInContext . fmap isRight) (\b _ -> fmap Right b)

instance Arbitrary a => Arbitrary (Matrix a) where
   arbitrary = do
      (i, j) <- arbitrary
      arbSizedMatrix (i `mod` 15, j `mod` 15)
   coarbitrary = coarbitrary . rows

instance Arbitrary a => Arbitrary (Context a) where -- can this be thrown away?
   arbitrary   = liftM inContext arbitrary
   coarbitrary = coarbitrary . fromContext

instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
   arbitrary = liftM fromInteger arbitrary
   coarbitrary r = coarbitrary (numerator r) . coarbitrary (denominator r)
   
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
   
newtype ShowRational = ShowRational Rational
   deriving (Eq, Num)

instance Show ShowRational where
   show (ShowRational r) = ppRational r