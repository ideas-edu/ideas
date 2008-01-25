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

import Prelude hiding (repeat)
import Domain.LinearAlgebra.Context
import Domain.LinearAlgebra.Rules
import Common.Strategy
import Common.Transformation

toReducedEchelon :: Fractional a => LabeledStrategy (MatrixInContext a)
toReducedEchelon = label "Gaussian elimination" $ 
   forwardPass <*> backwardPass

forwardPass :: Fractional a => LabeledStrategy (MatrixInContext a)
forwardPass = label "Forward pass" $ 
   repeat  $    label "Find j-th column"      ruleFindColumnJ 
           <*>  label "Exchange rows"         (try ruleExchangeNonZero)
           <*>  label "Scale row"             (try ruleScaleToOne)
           <*>  label "Zeros in j-th column"  (repeat ruleZerosFP)
           <*>  label "Cover up top row"      ruleCoverRow
  
backwardPass :: Fractional a => LabeledStrategy (MatrixInContext a)
backwardPass = label "Backward pass" $ 
   repeat  $    label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeat ruleZerosBP)