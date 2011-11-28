-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.MatrixRules where

import Common.Utils (readM)
import Common.Library hiding (simplify, isEmpty)
import Control.Monad
import Data.List
import Domain.LinearAlgebra.Matrix
import Domain.Math.Simplification

matrixRules :: (Argument a, Fractional a) => [Rule (Context (Matrix a))]
matrixRules =
   let noArgs f = f (const Nothing)
   in [ noArgs ruleScaleRow
      , noArgs ruleExchangeRows
      , noArgs ruleAddMultiple
      ]

ruleFindColumnJ :: Num a => Rule (Context (Matrix a))
ruleFindColumnJ = minorRule $ makeSimpleRule "linearalgebra.gaussianelim.FindColumnJ" $ withCM $ \m -> do
   cols <- liftM columns (subMatrix m)
   i    <- findIndexM nonZero cols
   writeVar columnJ i
   return m

ruleExchangeNonZero :: (Simplify a, Num a) => Rule (Context (Matrix a))
ruleExchangeNonZero = simplify $ ruleExchangeRows $ evalCM $ \m -> do
   nonEmpty m
   j   <- readVar columnJ
   col <- liftM (column j) (subMatrix m)
   i   <- findIndexM (/= 0) col
   cov <- readVar covered
   return (cov, i + cov)

ruleScaleToOne :: (Argument a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleScaleToOne = simplify $ ruleScaleRow $ evalCM $ \m -> do
   nonEmpty m
   j   <- readVar columnJ
   pv  <- liftM (entry (0, j)) (subMatrix m)
   guard (pv /= 0)
   cov <- readVar covered
   return (cov, 1 / pv)

ruleZerosFP :: (Argument a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosFP = simplify $ ruleAddMultiple $ evalCM $ \m -> do
   nonEmpty m
   j   <- readVar columnJ
   col <- liftM (drop 1 . column j) (subMatrix m)
   i   <- findIndexM (/= 0) col
   cov <- readVar covered
   let v = negate (col!!i)
   return (i + cov + 1, cov, v)

ruleZerosBP :: (Argument a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosBP = simplify $ ruleAddMultiple $ evalCM $ \m -> do
   nonEmpty m
   ri <- liftM (row 0) (subMatrix m)
   let j   = length $ takeWhile (==0) ri
       col = column j m
   guard (any (/= 0) ri)
   k <- findIndexM (/= 0) col
   let v = negate (col!!k)
   cov <- readVar covered
   return (k, cov, v)

ruleCoverRow :: Rule (Context (Matrix a))
ruleCoverRow = minorRule $ makeRule "linearalgebra.gaussianelim.CoverRow" $ changeCover succ

ruleUncoverRow :: Rule (Context (Matrix a))
ruleUncoverRow = minorRule $ makeRule "linearalgebra.gaussianelim.UncoverRow" $ changeCover pred

---------------------------------------------------------------------------------
-- Parameterized rules

ruleScaleRow :: (Argument a, Fractional a) => (Context (Matrix a) -> Maybe (Int, a)) -> Rule (Context (Matrix a))
ruleScaleRow f = makeRule "linearalgebra.gaussianelim.scale" (supply2 descr f rowScale)
 where descr  = ("row", "scale factor")

ruleExchangeRows :: Num a => (Context (Matrix a) -> Maybe (Int, Int)) -> Rule (Context (Matrix a))
ruleExchangeRows f = makeRule "linearalgebra.gaussianelim.exchange" (supply2 descr f rowExchange)
 where descr = ("row 1", "row 2")

ruleAddMultiple :: (Argument a, Fractional a) => (Context (Matrix a) -> Maybe (Int, Int, a)) -> Rule (Context (Matrix a))
ruleAddMultiple f = makeRule "linearalgebra.gaussianelim.add" (supply3 descr f  rowAdd)
 where descr  = ("row 1", "row2", "scale factor")

---------------------------------------------------------------------------------
-- Parameterized transformations

rowExchange :: Int -> Int -> Transformation (Context (Matrix a))
rowExchange i j = matrixTrans $ \m -> do
   guard (i /= j && validRow i m && validRow j m)
   return (switchRows i j m)

rowScale :: Num a => Int -> a -> Transformation (Context (Matrix a))
rowScale i k = matrixTrans $ \m -> do
   guard (k `notElem` [0, 1] && validRow i m)
   return (scaleRow i k m)

rowAdd :: Num a => Int -> Int -> a -> Transformation (Context (Matrix a))
rowAdd i j k = matrixTrans $ \m -> do
   guard (k /= 0 && i /= j && validRow i m && validRow j m)
   return (addRow i j k m)

changeCover :: (Int -> Int) -> Transformation (Context (Matrix a))
changeCover f = makeTrans $ withCM $ \m -> do
   new <- liftM f (readVar covered)
   guard (new >= 0 && new <= fst (dimensions m))
   writeVar covered new
   return m

matrixTrans ::  (Matrix a -> Maybe (Matrix a)) -> Transformation (Context (Matrix a))
matrixTrans f = makeTrans $ \c -> do
   a   <- fromContext c
   new <- f a
   return (replace new c)

-- local helper function
validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)

nonEmpty :: Matrix a -> ContextMonad ()
nonEmpty m = subMatrix m >>= guard . not . isEmpty

covered, columnJ :: Binding Int
covered = bindingParser readM $ emptyArgDescr "covered" 0 show
columnJ = bindingParser readM $ emptyArgDescr "columnj" 0 show

subMatrix :: Matrix a -> ContextMonad (Matrix a)
subMatrix m = do
   cov <- readVar covered
   return $ makeMatrix $ drop cov $ rows m

findIndexM :: MonadPlus m => (a -> Bool) -> [a] -> m Int
findIndexM p = maybe mzero return . findIndex p