-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Matrix.Rules where

import Matrix.Domain
import Matrix.Context
import Transformation
import Control.Monad
import Data.List

-- Quick hack
getRule :: Num a => String -> Rule (MatrixInContext a)
getRule s = case filter ((==s) . name) matrixRules of
               hd:_ -> hd
               _    -> error "getRule"

matrixRules :: Num a => [Rule (MatrixInContext a)]
matrixRules = 
   [ makeRule "RowExchange" $ selectArgs $ \c -> 
        ruleRowExchange (curRow c) (rowR c)
   , makeRule "RowScale" $ selectArgs $ \c -> 
        ruleRowScale (curRow c) (value c)
   , makeRule "RowAdd" $ selectArgs $ \c -> 
        ruleRowAdd (curRow c) (rowR c) (value c)
   , makeRule "CoverTop" $ \c ->
        return c {covered = covered c + 1}
   , makeRule "SetColumnJ" $ \c ->
        return c {columnJ = curColumn c}
   , makeRule "SetRowR" $ \c ->
        return c {rowR = curRow c}
   ]
 where
   selectArgs f c = do
      new <- apply (f c) (matrix c)
      return c {matrix = new}
      
ruleMoveToColJ :: Fractional a => Rule (MatrixInContext a)
ruleMoveToColJ = makeRule "_ToColumnJ" $ \c -> do
   let cols = Data.List.transpose $ drop (covered c) (rows $ matrix c)
   i <- findIndex (any (/= 0)) cols
   return c {curColumn = i}
 
ruleNormalizeRow :: Fractional a => Rule (MatrixInContext a)
ruleNormalizeRow = makeRule "_NormalizeRow" $ \c -> do
   let rs = filter (any (/= 0) . snd) $ filter ((/=[1]) . take 1 . dropWhile (==0) . snd) $ zip [0..] (rows $ matrix c)
       (n, row) = head rs
       v = 1 / head (dropWhile (==0) row)
   guard (not $ null rs)
   return c {curRow = n, value = v}

ruleFindSweep :: Fractional a => Rule (MatrixInContext a)
ruleFindSweep = makeRule "_FindSweep" $ \c -> do
   case sweeps (matrix c) of
      t@(i, j, v):_ -> return c {curRow = i, rowR = j, value = v}
      _ -> Nothing
      
sweeps :: Fractional a => Matrix a -> [(Int, Int, a)]
sweeps matrix = 
   [ (i, j , negate v) | (j, s) <- indexed, isNormalized s, (i, r) <- indexed, i < j
               , let v = r !! length (takeWhile (==0) s), v /= 0 ]
 where
   indexed = reverse $ zip [0..] $ rows matrix -- !!! REVERSE IS IMPORANT HERE
   isNormalized = (==[1]) . take 1 . dropWhile (==0)
   
ruleSetVal :: Fractional a => Rule (MatrixInContext a)
ruleSetVal = makeRule "_SetVal" $ \c -> do 
   let a = entry (curRow c, columnJ c) (matrix c)
       b = entry (rowR c, columnJ c) (matrix c)
       v = negate (a/b)
   return c {value = v}

ruleNonZeroR :: Fractional a => Rule (MatrixInContext a)
ruleNonZeroR = makeRule "_NonZeroR" $ \c -> do
   let col = map (!! columnJ c) $ drop (covered c) (rows $ matrix c)
   i <- findIndex (/= 0) col
   return c {curRow = covered c, rowR = i + covered c}
   
ruleMakeZero :: Fractional a => Rule (MatrixInContext a)
ruleMakeZero = makeRule "_MakeZero" $ \c -> do
   let col = map (!! columnJ c) $ drop (covered c+1) (rows $ matrix c)
   i <- findIndex (/= 0) col
   let v = negate (col!!i) / ((!! columnJ c) $ head $ drop (covered c) $ rows $ matrix c)
   return c {curRow = i + covered c + 1, rowR = covered c, value = v}
   
ruleRowExchange :: Int -> Int -> Rule (Matrix a)
ruleRowExchange i j = makeRule "RowExchange" (return . switchRows i j)
                                                                            
ruleRowScale :: Num a => Int -> a -> Rule (Matrix a)
ruleRowScale i k = makeRule "RowScale" (return . scaleRow i k)

ruleRowAdd :: Num a => Int -> Int -> a -> Rule (Matrix a)
ruleRowAdd i j k = makeRule "RowAdd" (return . addRow i j k)