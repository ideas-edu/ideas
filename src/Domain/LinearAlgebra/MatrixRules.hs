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
import Common.Context
import Common.Transformation
import Common.Utils
import Control.Monad
import Data.List

matrixRules :: Fractional a => [Rule (MatrixInContext a)]
matrixRules = 
   [ ruleExchangeNonZero, ruleScaleToOne, ruleFindColumnJ
   , ruleZerosFP, ruleZerosBP, ruleCoverRow, ruleUncoverRow
   ]

ruleFindColumnJ :: Num a => Rule (MatrixInContext a)
ruleFindColumnJ = minorRule $ makeSimpleRule "FindColumnJ" $ \c -> do
   let cols = columns (subMatrix c)
   i <- findIndex nonZero cols
   return (set columnJ i c)
   
ruleExchangeNonZero :: Num a => Rule (MatrixInContext a)
ruleExchangeNonZero = makeRule "ExchangeNonZero" (app2 rowExchange args)
 where
   args c = do
      nonEmpty c
      let col = column (get columnJ c) (subMatrix c)
      i   <- findIndex (/= 0) col
      return (get covered c, i + get covered c)

ruleScaleToOne :: Fractional a => Rule (MatrixInContext a)
ruleScaleToOne = makeRule "ScaleToOne" (app2 rowScale args)
 where
   args c = do
      nonEmpty c
      let pv = entry (0, get columnJ c) (subMatrix c)
      guard (pv /= 0)
      return (get covered c, 1 / pv)

ruleZerosFP :: Fractional a => Rule (MatrixInContext a)
ruleZerosFP = makeRule "Introduce zeros (forward pass)" (app3 rowAdd args)
 where
   args c = do
      nonEmpty c
      let col = drop 1 $ column (get columnJ c) (subMatrix c)
      i   <- findIndex (/= 0) col
      let v = negate (col!!i)
      return (i + get covered c + 1, get covered c, v)
   
ruleZerosBP :: Fractional a => Rule (MatrixInContext a)
ruleZerosBP = makeRule "Introduce zeros (backward pass)" (app3 rowAdd args)
 where
   args c = do
      nonEmpty c
      let ri  = row 0 (subMatrix c)
          j   = length $ takeWhile (==0) ri
          col = column j (matrix c)
      guard (any (/= 0) ri)
      k <- findIndex (/= 0) col
      let v = negate (col!!k)
      return (k, get covered c, v)

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
   let new = f (get covered c)
   guard (new >= 0 && new <= fst (dimensions (matrix c)))
   return $ set covered new c --  c {get covered = new}
   
matrixTrans :: (Matrix a -> Maybe (Matrix a)) -> Transformation (InContext (Matrix a))
matrixTrans f = makeTrans $ \c -> do
   new <- f (fromContext c)
   return (fmap (const new) c)

-- local helper function
validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)
   
nonEmpty :: MatrixInContext a -> Maybe ()
nonEmpty = guard . not . isEmpty . subMatrix

covered, columnJ :: Var Int
covered = "covered" := 0
columnJ = "columnJ" := 0

type MatrixInContext a = InContext (Matrix a)

matrix, subMatrix :: MatrixInContext a -> Matrix a
matrix = fromContext
subMatrix c = makeMatrix $ drop (get covered c) $ rows $ matrix c