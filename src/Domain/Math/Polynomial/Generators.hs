module Domain.Math.Polynomial.Generators 
   ( polynomialGen, cubicGen, quadraticGen, linearGen ) where

import Prelude hiding ((^))
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Numeric.Generators
import Test.QuickCheck
import Control.Monad

-- Random polynomial generator for (exactly) degree d
-- No division by zero
polynomialGen :: Int -> Int -> Gen Expr
polynomialGen d n
   | d==0         = ratGen
   | n==0 && d==1 = return (Var "x") 
   | n==0         = return (Var "x" ^ fromIntegral d) 
   | otherwise    = oneof $
        [ timesGen, plusGen
        , liftM2 (:/:) (rec d) ratGenNZ
        ] ++ [ powerGen | d > 1 ]
 where
   rec i = polynomialGen i (n `div` 2)
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
cubicGen     = polynomialGen 3
quadraticGen = polynomialGen 2
linearGen    = polynomialGen 1

ratGen, ratGenNZ :: Gen Expr
ratGen   = liftM fromRational (ratioGen 40 10)
ratGenNZ = liftM fromRational (ratioGenNonZero 40 10)