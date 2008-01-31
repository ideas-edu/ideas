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
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Common.Strategy hiding (not)
import Common.Transformation

import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.Equation

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
backwardPass =  label "Backward pass" $ 
   repeat  $    label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeat ruleZerosBP)

backSubstitutionSimple :: Fractional a => LabeledStrategy (EqsInContext a)
backSubstitutionSimple = label "Back substitution with equally many variables and equations" $
       label "Cover all equations" ruleCoverAllEquations
   <*> repeat (   label "Uncover one equation"  ruleUncoverEquation
              <*> label "Scale equation to one" (try ruleScaleEquation)
              <*> label "Back Substitution"     (repeat ruleBackSubstitution)
              )

backSubstitution :: Fractional a => LabeledStrategy (EqsInContext a)
backSubstitution = label "Back substitution" $ 
   ruleIdentifyFreeVariables <*> backSubstitutionSimple
   
systemToEchelonWithEEO :: Fractional a => LabeledStrategy (EqsInContext a)
systemToEchelonWithEEO = label "System to Echelon Form (EEO)" $
   repeat $   label "Drop (0=0) equation"       ruleDropEquation
          <|> label "Inconsistent system (0=1)" ruleInconsistentSystem
          <|> check (not . null . remaining)
          <*> label "Exchange equations"        (try ruleExchangeEquations)
          <*> label "Scale equation to one"     (option ruleScaleEquation)
          <*> label "Eliminate variable"        (repeat ruleEliminateVar)
          <*> label "Cover up first equation"   ruleCoverUpEquation

generalSolutionLinearSystem :: Fractional a => LabeledStrategy (EqsInContext a)
generalSolutionLinearSystem = label "General solution to a lineary system" $
   systemToEchelonWithEEO <*> backSubstitution


-- temp for testing
test = applyAll ruleInconsistentSystem $ EIC [0 :==: 1] 0

ex1a :: Equations (LinearExpr Rational)
ex1a = 
   [   x1 + 2*x2 + 3*x3 - x4   :==:  0 
   , 2*x1 + 3*x2 - x3 + 3*x4   :==:  0
   , 4*x4 + 6*x2 + x3 + 2*x4   :==:  0
   ]

x1 = var "x1"
x2 = var "x2"
x3 = var "x3"
x4 = var "x4"
x5 = var "x5"