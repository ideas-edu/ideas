-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Matrix.Strategies where

import Matrix.Context
import Matrix.Rules
import Strategy

toReducedEchelon :: Fractional a => Strategy (MatrixInContext a)
toReducedEchelon = toEchelon <*> reduceMatrix

toEchelon :: Fractional a => Strategy (MatrixInContext a)
toEchelon = repeatS (setVariableJ <*> swapNonZero <*> zerosInJ <*> coverup)
 where
   setVariableJ = ruleMoveToColJ <*> getRule "SetColumnJ"
   swapNonZero  = ruleNonZeroR   <*> getRule "RowExchange"
   zerosInJ     = repeatS (ruleMakeZero <*> getRule "RowAdd")
   coverup      = getRule "CoverTop"
   
reduceMatrix :: Fractional a => Strategy (MatrixInContext a)
reduceMatrix = repeatS normalizeRow <*> repeatS sweepRow
 where
    normalizeRow = ruleNormalizeRow <*> getRule "RowScale"     
    sweepRow     = ruleFindSweep    <*> getRule "RowAdd"