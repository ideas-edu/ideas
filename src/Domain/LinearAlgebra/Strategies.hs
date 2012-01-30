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
module Domain.LinearAlgebra.Strategies
   ( gaussianElimStrategy, linearSystemStrategy
   , gramSchmidtStrategy, systemWithMatrixStrategy
   , forwardPass
   ) where

import Common.Library hiding (simplify)
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.Vector
import Domain.Math.Expr
import Domain.Math.Simplification

gaussianElimStrategy :: LabeledStrategy (Context (Matrix Expr))
gaussianElimStrategy = label "Gaussian elimination" $
   forwardPass <*> backwardPass

forwardPass :: LabeledStrategy (Context (Matrix Expr))
forwardPass = label "Forward pass" $
   simplifyRule <*>
   repeatS  (   label "Find j-th column"      ruleFindColumnJ
           <*>  label "Exchange rows"         (try ruleExchangeNonZero)
           <*>  label "Scale row"             (try ruleScaleToOne)
           <*>  label "Zeros in j-th column"  (repeatS ruleZerosFP)
           <*>  label "Cover up top row"      ruleCoverRow
            )

backwardPass :: LabeledStrategy (Context (Matrix Expr))
backwardPass = label "Backward pass" $
   simplifyRule <*>
   repeatS  (   label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeatS ruleZerosBP)
            )

backSubstitutionSimple :: LabeledStrategy (Context (LinearSystem Expr))
backSubstitutionSimple =
   label "Back substitution with equally many variables and equations" $
       simplifyFirst
   <*> label "Cover all equations" ruleCoverAllEquations
   <*> repeatS (   label "Uncover one equation"  ruleUncoverEquation
               <*> label "Scale equation to one" (try ruleScaleEquation)
               <*> label "Back Substitution"     (repeatS ruleBackSubstitution)
               )

backSubstitution :: LabeledStrategy (Context (LinearSystem Expr))
backSubstitution = label "Back substitution" $
   ruleIdentifyFreeVariables <*> backSubstitutionSimple

systemToEchelonWithEEO :: LabeledStrategy (Context (LinearSystem Expr))
systemToEchelonWithEEO =
   label "System to Echelon Form (EEO)" $
   simplifyFirst <*>
   repeatS (  dropEquation
          <|> check hasRemaining
          <*> label "Exchange equations"        (try ruleExchangeEquations)
          <*> label "Scale equation to one"     (option ruleScaleEquation)
          <*> label "Eliminate variable"        (repeatS ruleEliminateVar)
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
       repeatS (useC dropEquation)
   <*> conv1
   <*> useC gaussianElimStrategy
   <*> conv2
   <*> repeatS (useC dropEquation)

gramSchmidtStrategy :: LabeledStrategy (Context (VectorSpace (Simplified Expr)))
gramSchmidtStrategy =
   label "Gram-Schmidt" $ repeatS $ label "Iteration" $
       label "Consider next vector"   ruleNext
   <*> label "Make vector orthogonal" (repeatS (ruleNextOrthogonal <*> try ruleOrthogonal))
   <*> label "Normalize"              (try ruleNormalize)

varVars :: Ref [Expr]
varVars = makeRef "variables"

simplifyFirst :: Rule (Context (LinearSystem Expr))
simplifyFirst = simplifySystem (idRule "simplify")

conv1 :: Rule (Context Expr)
conv1 = describe "Convert linear system to matrix" $
   ruleTrans (linId, "tomatrix") $ makeTransLiftContext $ \expr -> do
      ls   <- fromExpr expr
      let (m, vs) = systemToMatrix ls
      varVars := map Var vs
      return (toExpr (simplify (m :: Matrix Expr)))

conv2 :: Rule (Context Expr)
conv2 = describe "Convert matrix to linear system" $
   ruleTrans (linId, "frommatrix") $ makeTransLiftContext $ \expr -> do
      evs <- varVars :? []
      m   <- fromExpr expr
      let vs     = [ v | Var v <- evs ]
          linsys = matrixToSystemWith vs (m :: Matrix Expr)  
      return $ simplify $ toExpr linsys
      
hasRemaining :: Context (LinearSystem a) -> Bool
hasRemaining c = any (not . null) $ 
   evalEnvMonad (current c >>= remaining) (environment c)