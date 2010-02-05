-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Strategies 
   ( gaussianElimStrategy, linearSystemStrategy
   , gramSchmidtStrategy, systemWithMatrixStrategy
   , forwardPass
   , liftExpr
   ) where

import Prelude hiding (repeat)
import Domain.Math.Expr
import Domain.Math.Simplification
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Symbols ()
import Common.Apply
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Context
import Domain.LinearAlgebra.Vector

gaussianElimStrategy :: LabeledStrategy (Context (Matrix Expr))
gaussianElimStrategy = label "Gaussian elimination" $ 
   forwardPass <*> backwardPass

forwardPass :: LabeledStrategy (Context (Matrix Expr))
forwardPass = label "Forward pass" $ 
   simplifyRule <*>
   repeat   (   label "Find j-th column"      ruleFindColumnJ 
           <*>  label "Exchange rows"         (try ruleExchangeNonZero)
           <*>  label "Scale row"             (try ruleScaleToOne)
           <*>  label "Zeros in j-th column"  (repeat ruleZerosFP)
           <*>  label "Cover up top row"      ruleCoverRow
            )
  
backwardPass :: LabeledStrategy (Context (Matrix Expr))
backwardPass = label "Backward pass" $ 
   simplifyRule <*>
   repeat   (   label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeat ruleZerosBP)
            )

backSubstitutionSimple :: LabeledStrategy (Context (LinearSystem Expr))
backSubstitutionSimple =
   label "Back substitution with equally many variables and equations" $
       simplifyFirst
   <*> label "Cover all equations" ruleCoverAllEquations
   <*> repeat (   label "Uncover one equation"  ruleUncoverEquation
              <*> label "Scale equation to one" (try ruleScaleEquation)
              <*> label "Back Substitution"     (repeat ruleBackSubstitution)
              )

backSubstitution :: LabeledStrategy (Context (LinearSystem Expr))
backSubstitution = label "Back substitution" $ 
   ruleIdentifyFreeVariables <*> backSubstitutionSimple
   
systemToEchelonWithEEO :: LabeledStrategy (Context (LinearSystem Expr))
systemToEchelonWithEEO =
   label "System to Echelon Form (EEO)" $
   simplifyFirst <*>
   repeat  (  dropEquation
          <|> check (maybe False (not . null) . evalCM remaining)
          <*> label "Exchange equations"        (try ruleExchangeEquations)
          <*> label "Scale equation to one"     (option ruleScaleEquation)
          <*> label "Eliminate variable"        (repeat ruleEliminateVar)
          <*> label "Cover up first equation"   ruleCoverUpEquation
           )

dropEquation :: LabeledStrategy (Context (LinearSystem Expr))
dropEquation =
   label "Drop equations" $
          label "Inconsistent system (0=1)" ruleInconsistentSystem
      <|> label "Drop (0=0) equation"       ruleDropEquation

linearSystemStrategy :: LabeledStrategy (Context (LinearSystem Expr))
linearSystemStrategy = label "General solution to a linear system" $
   systemToEchelonWithEEO <*> backSubstitution

systemWithMatrixStrategy :: LabeledStrategy (Context Expr)
systemWithMatrixStrategy = label "General solution to a linear system (matrix approach)" $
       repeat (mapRules liftExpr dropEquation) 
   <*> conv1 
   <*> mapRules liftExpr gaussianElimStrategy 
   <*> conv2 
   <*> repeat (mapRules liftExpr dropEquation)

gramSchmidtStrategy :: LabeledStrategy (Context (VectorSpace (Simplified Expr)))
gramSchmidtStrategy =
   label "Gram-Schmidt" $ repeat $ label "Iteration" $
       label "Consider next vector"   ruleNext 
   <*> label "Make vector orthogonal" (repeat (ruleNextOrthogonal <*> try ruleOrthogonal)) 
   <*> label "Normalize"              (try ruleNormalize)

vars :: Var [String]
vars = newVar "variables" []

simplifyFirst :: Rule (Context (LinearSystem Expr))
simplifyFirst = simplifySystem idRule

conv1 :: Rule (Context Expr)
conv1 = makeSimpleRule "Linear system to matrix" $ withCM $ \expr -> do
   ls <- fromExpr expr
   let (m, vs) = systemToMatrix ls
   writeVar vars vs
   return (toExpr (simplify (m :: Matrix Expr)))
 
conv2 :: Rule (Context Expr)
conv2 = makeSimpleRule "Matrix to linear system" $ withCM $ \expr -> do
   vs <- readVar vars
   m  <- fromExpr expr
   let linsys = matrixToSystemWith vs (m :: Matrix Expr)
   a  <- fromContext $ applyD simplifyFirst $ inContext linsys
   return $ toExpr a

liftExpr :: IsExpr a => Rule (Context a) -> Rule (Context Expr)
liftExpr = liftRule (contextView exprView)