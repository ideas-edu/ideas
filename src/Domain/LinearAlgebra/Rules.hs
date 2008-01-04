-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Rules where

import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.Context
import Common.Transformation
import Common.Utils
import Control.Monad
import Data.List

matrixRules :: Fractional a => [Rule (MatrixInContext a)]
matrixRules = 
   [ruleSwapNonZero, ruleMakeZero, ruleCoverTop, ruleSetColumnJ, ruleNormalize, ruleSweep]

ruleSwapNonZero :: Num a => Rule (MatrixInContext a)
ruleSwapNonZero = makeRule "SwapNonZero" (app2 rowExchange args)
 where
   args c = do
      let col = column (columnJ c) (subMatrix c)
      i <- findIndex (/= 0) col
      return (covered c, i + covered c)

ruleMakeZero :: Fractional a => Rule (MatrixInContext a)
ruleMakeZero = makeRule "MakeZero" (app3 rowAdd args)
 where
   args c = do
      let col = drop 1 $ column (columnJ c) (subMatrix c)
      i <- findIndex (/= 0) col
      let v = negate (col!!i) / entry (0, columnJ c) (subMatrix c)
      return (i + covered c + 1, covered c, v)

ruleSetColumnJ :: Num a => Rule (MatrixInContext a)
ruleSetColumnJ = minorRule $ makeSimpleRule "SetColumnJ" $ \c -> do
   let cols = columns (subMatrix c)
   i <- findIndex nonZeroRow cols
   return c {columnJ = i}

ruleCoverTop :: Rule (MatrixInContext a)
ruleCoverTop = minorRule $ makeSimpleRule "CoverTop" $ \c ->
   return c {covered = covered c + 1}

ruleNormalize :: Fractional a => Rule (MatrixInContext a)
ruleNormalize = makeRule "Normalize" (app2 rowScale args)
 where
   args c = do  
      i  <- findIndex (maybe False (/= 1) . pivot) (rows $ matrix c)
      pv <- pivot (row i $ matrix c)
      return (i, 1 / pv)
   
ruleSweep :: Fractional a => Rule (MatrixInContext a)
ruleSweep = makeRule "Sweep" (app3 rowAdd args)
 where
   args c = safeHead $ sweeps $ matrix c

-- local helper function
sweeps :: Fractional a => Matrix a -> [(Int, Int, a)]
sweeps matrix = 
   [ (i, j , negate v) | (j, s) <- indexed, isNormalized s, (i, r) <- indexed, i < j
               , let v = r !! length (takeWhile (==0) s), v /= 0 ]
 where
   indexed = reverse $ zip [0..] $ rows matrix -- !!! REVERSE IS IMPORANT HERE
   isNormalized = (== Just 1) . pivot

---------------------------------------------------------------------------------
-- Parameterized transformations

rowExchange :: Int -> Int -> Transformation (MatrixInContext a)
rowExchange i j = matrixTrans $ \m -> do
   guard (i /= j)
   return (switchRows i j m)
                                                                            
rowScale :: Num a => Int -> a -> Transformation (MatrixInContext a)
rowScale i k = matrixTrans $ \m -> do
   guard (k /= 0 && validRow i m)
   return (scaleRow i k m)

rowAdd :: Num a => Int -> Int -> a -> Transformation (MatrixInContext a)
rowAdd i j k = matrixTrans $ \m -> do
   guard (k /= 0 && i /= j && validRow i m && validRow j m)
   return (addRow i j k m)

matrixTrans :: (Matrix a -> Maybe (Matrix a)) -> Transformation (MatrixInContext a)
matrixTrans f = makeTrans $ \c -> do
   new <- f (matrix c)
   return c {matrix = new}

-- local helper function
validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)