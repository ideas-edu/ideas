module Domain.Math.Polynomial.Generators 
   ( polynomialGen, polynomialDegreeGen
   , cubicGen, quadraticGen, linearGen 
   ) where

import Prelude hiding ((^))
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Numeric.Generators
import Test.QuickCheck
import Control.Monad

polynomialGen :: Int -> Gen Expr
polynomialGen n = do
   d <- choose (0, n `div` 5)
   polynomialDegreeGen d n

-- Random polynomial generator for (exactly) degree d
-- No division by zero
polynomialDegreeGen :: Int -> Int -> Gen Expr
polynomialDegreeGen d n
   | d==0         = ratGen
   | n==0 && d==1 = return (Var "x") 
   | n==0         = return (Var "x" ^ fromIntegral d) 
   | otherwise    = oneof $
        [ timesGen, plusGen
        , liftM2 (:/:) (rec d) ratGenNZ
        ] ++ [ powerGen | d > 1 ]
 where
   rec i = polynomialDegreeGen i (n `div` 2)
   plusGen = do
      d1 <- choose (0, d)
      a <- rec d1
      b <- rec d
      oneof $ map return [a :+: b, b :+: a, a :-: b, b :-: a, Negate b]
   timesGen = do
      d1 <- choose (0, d)
      a  <- rec d1
      b  <- rec (d-d1)
      return (a :*: b)
   powerGen = do
      i <- oneof $ [ return i | i <- [2..d], d `mod` i == 0 ]
      a <- rec (d `div` i)
      return (a ^ fromIntegral i)
      
cubicGen, quadraticGen, linearGen :: Int -> Gen Expr
cubicGen     = polynomialDegreeGen 3
quadraticGen = polynomialDegreeGen 2
linearGen    = polynomialDegreeGen 1

ratGen, ratGenNZ :: Gen Expr
ratGen   = sized ratioExprGen
ratGenNZ = sized ratioExprGenNonZero