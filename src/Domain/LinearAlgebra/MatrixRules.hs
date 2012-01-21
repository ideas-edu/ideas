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
   , covered, getCovered
   ) where

import Common.Library hiding (simplify, isEmpty)
import Control.Monad
import Data.List
import Domain.LinearAlgebra.Matrix
import Domain.Math.Simplification

ruleFindColumnJ :: Num a => Rule (Context (Matrix a))
ruleFindColumnJ = minorRule $ makeRule (gaussId, "FindColumnJ") $ 
   makeTransEnv $ same $ \m -> do
      cols <- liftM columns (subMatrix m)
      i    <- findIndexM nonZero cols   -- !! LIST
      columnJ := i

ruleExchangeNonZero :: (Simplify a, Num a) => Rule (Context (Matrix a))
ruleExchangeNonZero = simplify $ makeRule (gaussId, "exchange") $
   supplyParameters (fmap liftToContext rowExchange) $ useEnv $ \m -> do
      nonEmpty m
      j   <- getColumnJ
      col <- liftM (column j) (subMatrix m)
      i   <- findIndexM (/= 0) col
      cov <- getCovered
      return (cov, i + cov)

ruleScaleToOne :: (Reference a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleScaleToOne = simplify $ makeRule (gaussId, "scale") $
   supplyParameters (fmap liftToContext rowScale) $ useEnv $ \m -> do
      nonEmpty m
      j   <- getColumnJ
      cov <- getCovered
      pv  <- liftM (entry (0, j)) (subMatrix m)
      guard (pv /= 0)
      return (cov, 1 / pv)

ruleZerosFP :: (Reference a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosFP = simplify $ makeRule (gaussId, "add") $
   supplyParameters (fmap liftToContext rowAdd) $ useEnv $ \m -> do
      nonEmpty m
      j   <- getColumnJ
      col <- liftM (drop 1 . column j) (subMatrix m)
      i   <- findIndexM (/= 0) col
      cov <- getCovered
      let v = negate (col!!i)
      return (i + cov + 1, cov, v)

ruleZerosBP :: (Reference a, Simplify a, Fractional a) => Rule (Context (Matrix a))
ruleZerosBP = simplify $ makeRule (gaussId, "add") $
   supplyParameters (fmap liftToContext rowAdd) $ useEnv $ \m -> do
      nonEmpty m
      ri <- liftM (row 0) (subMatrix m)
      let j   = length $ takeWhile (==0) ri
          col = column j m
      guard (any (/= 0) ri)
      k <- findIndexM (/= 0) col
      let v = negate (col!!k)
      cov <- getCovered
      return (k, cov, v)

ruleCoverRow :: Rule (Context (Matrix a))
ruleCoverRow = minorRule $ makeRule (gaussId, "CoverRow") $ changeCover succ

ruleUncoverRow :: Rule (Context (Matrix a))
ruleUncoverRow = minorRule $ makeRule (gaussId, "UncoverRow") $ changeCover pred

---------------------------------------------------------------------------------
-- Parameterized transformations

rowExchange :: ParamTrans (Int, Int) (Matrix a)
rowExchange = parameter2 "row1" "row2" $ \i j -> 
   makeTrans $ \m -> do
      guard (i /= j && validRow i m && validRow j m)
      return (switchRows i j m)

rowScale :: (Reference a, Num a) => ParamTrans (Int, a) (Matrix a)
rowScale = parameter2 "row" "scale factor" $ \i k -> 
   makeTrans $ \m -> do
      guard (k `notElem` [0, 1] && validRow i m)
      return (scaleRow i k m)

rowAdd :: (Reference a, Num a) => ParamTrans (Int, Int, a) (Matrix a)
rowAdd = parameter3 "row1" "row2" "scale factor" $ \i j k -> 
   makeTrans $ \m -> do
      guard (k /= 0 && i /= j && validRow i m && validRow j m)
      return (addRow i j k m)

changeCover :: (Int -> Int) -> Transformation (Context (Matrix a))
changeCover f = makeTransEnv $ same $ \m -> do
   new <- liftM f getCovered
   guard (new >= 0 && new <= fst (dimensions m))
   covered := new

-- local helper function
same :: (a -> EnvMonad b) -> a -> EnvMonad a
same f a = f a >> return a

useEnv :: (a -> EnvMonad b) -> Context a -> Maybe b
useEnv f c = current c >>= \a -> evalEnvMonad (f a) (environment c)

validRow :: Int -> Matrix a -> Bool
validRow i m = i >= 0 && i < fst (dimensions m)

nonEmpty :: Matrix a -> EnvMonad ()
nonEmpty m = subMatrix m >>= guard . not . isEmpty

covered, columnJ :: Ref Int
covered = makeRef "covered"
columnJ = makeRef "columnj"

getCovered, getColumnJ :: EnvMonad Int
getCovered = covered :? 0
getColumnJ = columnJ :? 0

subMatrix :: Matrix a -> EnvMonad (Matrix a)
subMatrix m = do
    cov <- getCovered
    return $ makeMatrix $ drop cov $ rows m

findIndexM :: MonadPlus m => (a -> Bool) -> [a] -> m Int
findIndexM p = maybe mzero return . findIndex p

gaussId :: Id
gaussId = newId "linearalgebra.gaussianelim"