-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Numeric.Generators 
   ( integerGenerator, rationalGenerator, numGenerator
   , ratioGen, ratioExprGen, ratioExprGenNonZero, nonZero
   ) where

import Control.Monad
import Common.View
import Data.Ratio
import Domain.Math.Numeric.Views
import Test.QuickCheck
import Domain.Math.Expr

-------------------------------------------------------------------
-- Generators

-- tailored towards generating "int" expressions (also prevents 
-- division by zero)
integerGenerator :: Int -> Gen Expr
integerGenerator = symbolGenerator extras numSymbols
 where
   extras n = natGenerator : [ divGen n | n > 0 ]
   divGen n = do
      e1 <- integerGenerator (n `div` 2)
      e2 <- integerGenerator (n `div` 2)
      case (match integerView e1, match integerView e2) of
         (Just a, Just b) 
            | b == 0 -> oneof $ map return
                 [ e1 :/: (e2 + 1), e1 :/: (e2 - 1)
                 , e1 :/: (1 + e2), e1 :/: (1 - e2) 
                 ]
            | a `mod` b == 0 ->
                 return (e1 :/: e2)
            | otherwise -> do -- change numerator
                i <- arbitrary
                let m1 = fromInteger ((a `mod` b) + i*b)
                    m2 = fromInteger (b - (a `mod` b) + i*b)
                oneof $ map return 
                   [ (e1 - m1) :/: e2, (m1 - e1) :/: e2
                   , (e1 + m2) :/: e2, (m2 + e1) :/: e2
                   ]
         _ -> error "integerGenerator"

-- Prevents division by zero
rationalGenerator :: Int -> Gen Expr
rationalGenerator = symbolGenerator extras numSymbols
 where
   extras n = natGenerator : [ divGen n | n > 0 ]
   divGen n = do
      e1 <- rationalGenerator (n `div` 2)
      e2 <- rationalGenerator (n `div` 2)
      case match rationalView e2 of 
         Just b | b == 0 -> return e1
         _               -> return (e1 :/: e2)

-- Also generates "division-by-zero" expressions
numGenerator :: Int -> Gen Expr
numGenerator = symbolGenerator (const [natGenerator]) $ 
   (divideSymbol, Just 2):numSymbols

ratioExprGen :: Int -> Gen Expr
ratioExprGen n = liftM fromRational $ ratioGen n (n `div` 4)

ratioExprGenNonZero :: Int -> Gen Expr
ratioExprGenNonZero n = liftM fromRational $ nonZero $ ratioGen n (n `div` 4)

nonZero :: Num a => Gen a -> Gen a
nonZero = liftM (\a -> if a==0 then 1 else a)

numSymbols :: [(Symbol, Maybe Int)]
numSymbols = (negateSymbol, Just 1)
           : zip [plusSymbol, timesSymbol, minusSymbol] (repeat (Just 2))
           
-------------------------------------------------------------------
-- Helpers

symbolGenerator :: (Int -> [Gen Expr]) -> [(Symbol, Maybe Int)] -> Int -> Gen Expr
symbolGenerator extras syms = f 
 where
   f n = oneof $  map (g n) (filter (\(_, a) -> n > 0 || a == Just 0) syms)
               ++ extras n
   g n (s, arity) = do
      i  <- case arity of
               Just i  -> return i
               Nothing -> choose (0, 5)
      as <- replicateM i (f (n `div` i))
      return (function s as)
  
natGenerator :: Gen Expr
natGenerator = liftM (Nat . abs) arbitrary

-- | Prevents a bias towards small numbers
ratioGen :: Integral a => Int -> Int -> Gen (Ratio a)
ratioGen n m = do 
   a <- choose (-n, n)
   b <- liftM (succ . abs) (choose (-m, m))
   c <- choose (1-b, b-1)
   return (fromIntegral a + (fromIntegral c / fromIntegral b))