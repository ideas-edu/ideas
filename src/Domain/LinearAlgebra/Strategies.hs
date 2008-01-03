-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Strategies where

import Domain.LinearAlgebra.Context
import Domain.LinearAlgebra.Rules
import Common.Strategy
import Common.Transformation

toReducedEchelon :: Fractional a => Strategy (MatrixInContext a)
toReducedEchelon = toEchelon <*> reduceMatrix

toEchelon :: Fractional a => Strategy (MatrixInContext a)
toEchelon = repeatS (ruleSetColumnJ <*> try ruleSwapNonZero <*> repeatS ruleMakeZero <*> ruleCoverTop)
   
reduceMatrix :: Fractional a => Strategy (MatrixInContext a)
reduceMatrix = repeatS ruleNormalize <*> repeatS ruleSweep