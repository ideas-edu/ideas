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

import Common.Library hiding (simplify, isEmpty)
import Common.Results
import Control.Monad
import Data.List
import Domain.LinearAlgebra.Matrix
import Domain.Math.Simplification

ruleFindColumnJ :: Num a => Rule (Context (Matrix a))
ruleFindColumnJ = minorRule $ makeSimpleRuleList "linearalgebra.gaussianelim.FindColumnJ" $ withCM $ \m -> do
   cols <- liftM columns (subMatrix m)
   i    <- findIndexM nonZero cols
   writeVar columnJ i
   return m

ruleExchangeNonZero :: (Simplify a, Num a) => Rule (Context (Matrix a))
ruleExchangeNonZero = simplify $ ruleExchangeRows $ \m -> do
   nonEmpty m
   j   <- readVar columnJ
   col <- liftM (column j) (subMatrix m)
   i   <- findIndexM (/= 0) col
   cov <- readVar covered
   return (cov, i + cov)

ruleScaleToOne :: (Bindable a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleScaleToOne = simplify $ ruleScaleRow $ \m -> do
   nonEmpty m
   j   <- readVar columnJ
   pv  <- liftM (entry (0, j)) (subMatrix m)
   guard (pv /= 0)
   cov <- readVar covered
   return (cov, 1 / pv)

ruleZerosFP :: (Bindable a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosFP = simplify $ ruleAddMultiple $ \m -> do
   nonEmpty m
   j   <- readVar columnJ
   col <- liftM (drop 1 . column j) (subMatrix m)
   i   <- findIndexM (/= 0) col
   cov <- readVar covered
   let v = negate (col!!i)
   return (i + cov + 1, cov, v)

ruleZerosBP :: (Bindable a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosBP = simplify $ ruleAddMultiple $ \m -> do
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

ruleScaleRow :: (Bindable a, Fractional a) => (Matrix a -> Results (Int, a)) -> Rule (Context (Matrix a))
ruleScaleRow f = makeRule "linearalgebra.gaussianelim.scale" $ 
   supplyParameters rowScale (evalCM f)

ruleExchangeRows :: Num a => (Matrix a -> Results (Int, Int)) -> Rule (Context (Matrix a))
ruleExchangeRows f = makeRule "linearalgebra.gaussianelim.exchange" $
   supplyParameters rowExchange (evalCM f) 

ruleAddMultiple :: (Bindable a, Fractional a) => (Matrix a -> Results (Int, Int, a)) -> Rule (Context (Matrix a))
ruleAddMultiple f = makeRule "linearalgebra.gaussianelim.add" $
   supplyParameters rowAdd (evalCM f)

---------------------------------------------------------------------------------
-- Parameterized transformations

rowExchange :: ParamTrans (Int, Int) (Context (Matrix a))
rowExchange = parameter2 "row1" "row2" $ \i j -> matrixTrans $ \m -> do
   guard (i /= j && validRow i m && validRow j m)
   return (switchRows i j m)

rowScale :: (Bindable a, Num a) => ParamTrans (Int, a) (Context (Matrix a))
rowScale = parameter2 "row" "scale factor" $ \i k -> matrixTrans $ \m -> do
   guard (k `notElem` [0, 1] && validRow i m)
   return (scaleRow i k m)

rowAdd :: (Bindable a, Num a) => ParamTrans (Int, Int, a) (Context (Matrix a))
rowAdd = parameter3 "row1" "row2" "scale factor" $ \i j k -> matrixTrans $ \m -> do
   guard (k /= 0 && i /= j && validRow i m && validRow j m)
   return (addRow i j k m)

changeCover :: (Int -> Int) -> Transformation (Context (Matrix a))
changeCover f = makeTransG $ withCM $ \m -> do
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

nonEmpty :: Matrix a -> Results ()
nonEmpty m = subMatrix m >>= guard . not . isEmpty

covered, columnJ :: Binding Int
covered = "covered" .<-. 0
columnJ = "columnj" .<-. 0

subMatrix :: Matrix a -> Results (Matrix a)
subMatrix m = do
   cov <- readVar covered
   return $ makeMatrix $ drop cov $ rows m

findIndexM :: MonadPlus m => (a -> Bool) -> [a] -> m Int
findIndexM p = maybe mzero return . findIndex p