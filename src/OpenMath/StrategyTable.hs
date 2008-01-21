module OpenMath.StrategyTable where

import Common.Assignment
import Domain.LinearAlgebra.Checks (reduceMatrixAssignment)
import Domain.LinearAlgebra
import OpenMath.ObjectParser

type StrategyID = String
type Location   = [Int]

-- not yet used
strategyTable :: [(String, PackedAssignment)]
strategyTable =
   [ ("Gaussian Elimination", Pack reduceMatrixAssignment)
   ]

instance IsExpr a => IsExpr (Matrix a) where
   toExpr   = Matrix . map (map toExpr) . rows
   fromExpr (Matrix xs) = do 
      rows <- mapM (mapM fromExpr) xs
      return $ makeMatrix rows
   fromExpr _ = Nothing