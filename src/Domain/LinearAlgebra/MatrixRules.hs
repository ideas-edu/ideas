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
module Domain.LinearAlgebra.MatrixRules where

import Domain.LinearAlgebra.Matrix
import Common.Context
import Common.Transformation
import Control.Monad
import Data.List

matrixRules :: (Argument a, Fractional a) => [Rule (Context (Matrix a))]
matrixRules = 
   let noArgs f = f (\_ -> Nothing)
   in [ noArgs ruleScaleRow, noArgs ruleExchangeRows, noArgs ruleAddMultiple ]
-- ruleExchangeNonZero, ruleScaleToOne, ruleFindColumnJ
     -- , ruleZerosFP, ruleZerosBP, ruleCoverRow, ruleUncoverRow

ruleFindColumnJ :: Num a => Rule (Context (Matrix a))
ruleFindColumnJ = minorRule $ makeSimpleRule "FindColumnJ" $ \c -> do
   let cols = columns (subMatrix c)
   i <- findIndex nonZero cols
   return (set columnJ i c)
   
ruleExchangeNonZero :: Num a => Rule (Context (Matrix a))
ruleExchangeNonZero = ruleExchangeRows $ \c -> do
   nonEmpty c
   let col = column (get columnJ c) (subMatrix c)
   i   <- findIndex (/= 0) col
   return (get covered c, i + get covered c)

ruleScaleToOne :: (Argument a, Fractional a) => Rule (Context (Matrix a))
ruleScaleToOne = ruleScaleRow $ \c -> do
   nonEmpty c
   let pv = entry (0, get columnJ c) (subMatrix c)
   guard (pv /= 0)
   return (get covered c, 1 / pv)

ruleZerosFP :: (Argument a, Fractional a) => Rule (Context (Matrix a))
ruleZerosFP = ruleAddMultiple $ \c -> do
   nonEmpty c
   let col = drop 1 $ column (get columnJ c) (subMatrix c)
   i   <- findIndex (/= 0) col
   let v = negate (col!!i)
   return (i + get covered c + 1, get covered c, v)
   
ruleZerosBP :: (Argument a, Fractional a) => Rule (Context (Matrix a))
ruleZerosBP = ruleAddMultiple $ \c -> do
   nonEmpty c
   let ri  = row 0 (subMatrix c)
       j   = length $ takeWhile (==0) ri
       col = column j (matrix c)
   guard (any (/= 0) ri)
   k <- findIndex (/= 0) col
   let v = negate (col!!k)
   return (k, get covered c, v)

ruleCoverRow :: Rule (Context (Matrix a))
ruleCoverRow = minorRule $ makeRule "CoverRow" $ changeCover (+1)

ruleUncoverRow :: Rule (Context (Matrix a))
ruleUncoverRow = minorRule $ makeRule "UncoverRow" $ changeCover (\x -> x-1)

---------------------------------------------------------------------------------
-- Parameterized rules

ruleScaleRow :: (Argument a, Fractional a) => (Context (Matrix a) -> Maybe (Int, a)) -> Rule (Context (Matrix a))
ruleScaleRow f = makeRule "Scale" (supplyLabeled2 descr f rowScale)
 where descr  = ("row", "scale factor")
      
ruleExchangeRows :: Num a => (Context (Matrix a) -> Maybe (Int, Int)) -> Rule (Context (Matrix a))
ruleExchangeRows f = makeRule "Exchange" (supplyLabeled2 descr f rowExchange)
 where descr = ("row 1", "row 2")

ruleAddMultiple :: (Argument a, Fractional a) => (Context (Matrix a) -> Maybe (Int, Int, a)) -> Rule (Context (Matrix a))
ruleAddMultiple f = makeRule "Add" (supplyLabeled3 descr f  rowAdd)
 where descr  = ("row 1", "row2", "scale factor")
      
---------------------------------------------------------------------------------
-- Parameterized transformations

rowExchange :: Int -> Int -> Transformation (Context (Matrix a))
rowExchange i j = matrixTrans "rowExchange" $ \m -> do
   guard (i /= j && validRow i m && validRow j m)
   return (switchRows i j m)
                                                                            
rowScale :: Num a => Int -> a -> Transformation (Context (Matrix a))
rowScale i k = matrixTrans "rowScale" $ \m -> do
   guard (k `notElem` [0, 1] && validRow i m)
   return (scaleRow i k m)

rowAdd :: Num a => Int -> Int -> a -> Transformation (Context (Matrix a))
rowAdd i j k = matrixTrans "rowAdd" $ \m -> do
   guard (k /= 0 && i /= j && validRow i m && validRow j m)
   return (addRow i j k m)

changeCover :: (Int -> Int) -> Transformation (Context (Matrix a))
changeCover f = makeTrans "changeCover" $ \c -> do
   let new = f (get covered c)
   guard (new >= 0 && new <= fst (dimensions (matrix c)))
   return $ set covered new c --  c {get covered = new}
   
matrixTrans ::  String -> (Matrix a -> Maybe (Matrix a)) -> Transformation (Context (Matrix a))
matrixTrans s f = makeTrans s $ \c -> do
   new <- f (fromContext c)
   return (fmap (const new) c)

-- local helper function
validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)
   
nonEmpty :: Context (Matrix a) -> Maybe ()
nonEmpty = guard . not . isEmpty . subMatrix

covered, columnJ :: Var Int
covered = "covered" := 0
columnJ = "columnJ" := 0

matrix, subMatrix :: Context (Matrix a) -> Matrix a
matrix = fromContext
subMatrix c = makeMatrix $ drop (get covered c) $ rows $ matrix c