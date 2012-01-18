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
module Domain.LinearAlgebra.MatrixRules 
   ( ruleFindColumnJ, ruleExchangeNonZero, ruleScaleToOne
   , ruleZerosFP, ruleCoverRow, ruleUncoverRow, ruleZerosBP
   , covered
   ) where

import Common.Library hiding (simplify, isEmpty)
import Control.Monad
import Data.List
import Domain.LinearAlgebra.Matrix
import Domain.Math.Simplification

ruleFindColumnJ :: Num a => Rule (Context (Matrix a))
ruleFindColumnJ = minorRule $ makeSimpleRuleList "linearalgebra.gaussianelim.FindColumnJ" $ \cm -> do
   cols <- liftM columns (subMatrix cm)
   i    <- findIndexM nonZero cols
   return (writeVar columnJ i cm)

ruleExchangeNonZero :: (Simplify a, Num a) => Rule (Context (Matrix a))
ruleExchangeNonZero = simplify $ ruleExchangeRows $ \cm -> do
   guard (nonEmpty cm)
   let j = readVar columnJ cm
   col <- liftM (column j) (subMatrix cm)
   i   <- findIndexM (/= 0) col
   let cov = readVar covered cm
   return (cov, i + cov)

ruleScaleToOne :: (Bindable a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleScaleToOne = simplify $ ruleScaleRow $ \cm -> do
   guard (nonEmpty cm)
   let j = readVar columnJ cm
   pv  <- liftM (entry (0, j)) (subMatrix cm)
   guard (pv /= 0)
   let cov = readVar covered cm
   return (cov, 1 / pv)

ruleZerosFP :: (Bindable a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosFP = simplify $ ruleAddMultiple $ \cm -> do
   guard (nonEmpty cm)
   let j = readVar columnJ cm
   col <- liftM (drop 1 . column j) (subMatrix cm)
   i   <- findIndexM (/= 0) col
   let cov = readVar covered cm
       v = negate (col!!i)
   return (i + cov + 1, cov, v)

ruleZerosBP :: (Bindable a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosBP = simplify $ ruleAddMultiple $ \cm -> do
   m <- fromContext cm
   guard (nonEmpty cm)
   ri <- liftM (row 0) (subMatrix cm)
   let j   = length $ takeWhile (==0) ri
       col = column j m
   guard (any (/= 0) ri)
   k <- findIndexM (/= 0) col
   let v = negate (col!!k)
       cov = readVar covered cm
   return (k, cov, v)

ruleCoverRow :: Rule (Context (Matrix a))
ruleCoverRow = minorRule $ makeRule "linearalgebra.gaussianelim.CoverRow" $ changeCover succ

ruleUncoverRow :: Rule (Context (Matrix a))
ruleUncoverRow = minorRule $ makeRule "linearalgebra.gaussianelim.UncoverRow" $ changeCover pred

---------------------------------------------------------------------------------
-- Parameterized rules

ruleScaleRow :: (Bindable a, Fractional a) => (Context (Matrix a) -> Maybe (Int, a)) -> Rule (Context (Matrix a))
ruleScaleRow = makeRule "linearalgebra.gaussianelim.scale" .
   supplyParameters rowScale

ruleExchangeRows :: Num a => (Context (Matrix a) -> Maybe (Int, Int)) -> Rule (Context (Matrix a))
ruleExchangeRows = makeRule "linearalgebra.gaussianelim.exchange" .
   supplyParameters rowExchange

ruleAddMultiple :: (Bindable a, Fractional a) => (Context (Matrix a) -> Maybe (Int, Int, a)) -> Rule (Context (Matrix a))
ruleAddMultiple = makeRule "linearalgebra.gaussianelim.add" .
   supplyParameters rowAdd

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
changeCover f = makeTrans $ \cm -> do
   m   <- fromContext cm
   let new = f (readVar covered cm)
   guard (new >= 0 && new <= fst (dimensions m))
   return (writeVar covered new cm)

matrixTrans ::  (Matrix a -> Maybe (Matrix a)) -> Transformation (Context (Matrix a))
matrixTrans f = makeTrans $ \c -> do
   a   <- fromContext c
   new <- f a
   return (replace new c)

-- local helper function
validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)

nonEmpty :: Context (Matrix a) -> Bool
nonEmpty = maybe False (not . isEmpty) . subMatrix

covered, columnJ :: Binding Int
covered = "covered" .<-. 0
columnJ = "columnj" .<-. 0

subMatrix :: Context (Matrix a) -> Maybe (Matrix a)
subMatrix cm = do
   m <- fromContext cm
   let cov = readVar covered cm
   return $ makeMatrix $ drop cov $ rows m

findIndexM :: MonadPlus m => (a -> Bool) -> [a] -> m Int
findIndexM p = maybe mzero return . findIndex p