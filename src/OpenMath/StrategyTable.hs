module OpenMath.StrategyTable where

import Common.Assignment
import Domain.LinearAlgebra.Checks (reduceMatrixAssignment)

type StrategyID = String
type Location   = [Int]

-- not yet used
strategyTable :: [(String, PackedAssignment)]
strategyTable =
   [ ("Gaussian Elimination", Pack reduceMatrixAssignment)
   ]