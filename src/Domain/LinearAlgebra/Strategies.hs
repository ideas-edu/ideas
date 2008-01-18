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

toReducedEchelon :: Fractional a => NamedStrategy (MatrixInContext a)
toReducedEchelon = label "Gaussian elimination" $ 
   forwardPass <*> backwardPass

forwardPass :: Fractional a => NamedStrategy (MatrixInContext a)
forwardPass = label "Forward pass" $ 
   repeatNS  $    label "Find j-th column"      ruleFindColumnJ 
             <*>  label "Exchange rows"         (try ruleExchangeNonZero)
             <*>  label "Scale row"             (try ruleScaleToOne)
             <*>  label "Zeros in j-th column"  (repeatS ruleZerosFP)
             <*>  label "Cover up top row"      ruleCoverRow
  
backwardPass :: Fractional a => NamedStrategy (MatrixInContext a)
backwardPass = label "Backward pass" $ 
   repeatNS  $    label "Uncover row"  ruleUncoverRow
             <*>  label "Sweep"        (repeatS ruleZerosBP)