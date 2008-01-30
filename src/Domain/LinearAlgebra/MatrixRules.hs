-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.MatrixRules where

import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.Context
import Common.Transformation
import Common.Utils
import Control.Monad
import Data.List

matrixRules :: Fractional a => [Rule (MatrixInContext a)]
matrixRules = 
   [ruleExchangeNonZero, ruleScaleToOne, ruleFindColumnJ
   , ruleZerosFP, ruleZerosBP, ruleCoverRow, ruleUncoverRow
   ]

ruleFindColumnJ :: Num a => Rule (MatrixInContext a)
ruleFindColumnJ = minorRule $ makeSimpleRule "FindColumnJ" $ \c -> do
   let cols = columns (subMatrix c)
   i <- findIndex nonZero cols
   return c {columnJ = i}
   
ruleExchangeNonZero :: Num a => Rule (MatrixInContext a)
ruleExchangeNonZero = makeRule "ExchangeNonZero" (app2 rowExchange args)
 where
   args c = do
      let col = column (columnJ c) (subMatrix c)
      i <- findIndex (/= 0) col
      return (covered c, i + covered c)

ruleScaleToOne :: Fractional a => Rule (MatrixInContext a)
ruleScaleToOne = makeRule "ScaleToOne" (app2 rowScale args)
 where
   args c = do
      let pv = entry (0, columnJ c) (subMatrix c)
      return (covered c, 1 / pv)

ruleZerosFP :: Fractional a => Rule (MatrixInContext a)
ruleZerosFP = makeRule "Introduce zeros (forward pass)" (app3 rowAdd args)
 where
   args c = do
      let col = drop 1 $ column (columnJ c) (subMatrix c)
      i <- findIndex (/= 0) col
      let v = negate (col!!i)
      return (i + covered c + 1, covered c, v)
   
ruleZerosBP :: Fractional a => Rule (MatrixInContext a)
ruleZerosBP = makeRule "Introduce zeros (backward pass)" (app3 rowAdd args)
 where
   args c = do
      let ri = row 0 (subMatrix c)
          j  = length $ takeWhile (==0) ri
          col = column j (matrix c)
      k <- findIndex (/= 0) col
      let v = negate (col!!k)
      return (k, covered c, v)

ruleCoverRow :: Rule (MatrixInContext a)
ruleCoverRow = minorRule $ makeRule "CoverRow" $ changeCover (+1)

ruleUncoverRow :: Rule (MatrixInContext a)
ruleUncoverRow = minorRule $ makeRule "UncoverRow" $ changeCover (\x -> x-1)

---------------------------------------------------------------------------------
-- Parameterized transformations

rowExchange :: Int -> Int -> Transformation (MatrixInContext a)
rowExchange i j = matrixTrans $ \m -> do
   guard (i /= j)
   return (switchRows i j m)
                                                                            
rowScale :: Num a => Int -> a -> Transformation (MatrixInContext a)
rowScale i k = matrixTrans $ \m -> do
   guard (k `notElem` [0, 1] && validRow i m)
   return (scaleRow i k m)

rowAdd :: Num a => Int -> Int -> a -> Transformation (MatrixInContext a)
rowAdd i j k = matrixTrans $ \m -> do
   guard (k /= 0 && i /= j && validRow i m && validRow j m)
   return (addRow i j k m)

changeCover :: (Int -> Int) -> Transformation (MatrixInContext a)
changeCover f = makeTrans $ \c -> do
   let new = f (covered c)
   guard (new >= 0 && new <= fst (dimensions (matrix c)))
   return c {covered = new}
   
matrixTrans :: (Matrix a -> Maybe (Matrix a)) -> Transformation (MatrixInContext a)
matrixTrans f = makeTrans $ \c -> do
   new <- f (matrix c)
   return c {matrix = new}

-- local helper function
validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)