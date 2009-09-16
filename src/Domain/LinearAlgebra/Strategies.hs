-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Strategies where

import Prelude hiding (repeat)
import Domain.Math.Expr
import Domain.Math.Simplification
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearView (linearView)
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Context
import Common.View hiding (simplify)
import Domain.LinearAlgebra.Vector

toReducedEchelon :: LabeledStrategy (Context (Matrix Expr))
toReducedEchelon = label "Gaussian elimination" $ 
   forwardPass <*> backwardPass

forwardPass :: LabeledStrategy (Context (Matrix Expr))
forwardPass = cleanUpStrategy simplify $
   label "Forward pass" $ 
   repeat  $    label "Find j-th column"      ruleFindColumnJ 
           <*>  label "Exchange rows"         (try ruleExchangeNonZero)
           <*>  label "Scale row"             (try ruleScaleToOne)
           <*>  label "Zeros in j-th column"  (repeat ruleZerosFP)
           <*>  label "Cover up top row"      ruleCoverRow
  
backwardPass :: LabeledStrategy (Context (Matrix Expr))
backwardPass =  cleanUpStrategy simplify $
   label "Backward pass" $ 
   repeat  $    label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeat ruleZerosBP)

-- simplify a linear system
simplifySystem :: LinearSystem Expr -> LinearSystem Expr
simplifySystem = map (fmap f) 
 where f = simplifyWith (fmap simplify) linearView

backSubstitutionSimple :: LabeledStrategy (Context (LinearSystem Expr))
backSubstitutionSimple = cleanUpStrategy (fmap simplifySystem) $
   label "Back substitution with equally many variables and equations" $
       label "Cover all equations" ruleCoverAllEquations
   <*> repeat (   label "Uncover one equation"  ruleUncoverEquation
              <*> label "Scale equation to one" (try ruleScaleEquation)
              <*> label "Back Substitution"     (repeat ruleBackSubstitution)
              )

backSubstitution :: LabeledStrategy (Context (LinearSystem Expr))
backSubstitution = label "Back substitution" $ 
   ruleIdentifyFreeVariables <*> backSubstitutionSimple
   
systemToEchelonWithEEO :: LabeledStrategy (Context (LinearSystem Expr))
systemToEchelonWithEEO = cleanUpStrategy (fmap simplifySystem) $
   label "System to Echelon Form (EEO)" $
   repeat $   dropEquation
          <|> check (not . null . remaining)
          <*> label "Exchange equations"        (try ruleExchangeEquations)
          <*> label "Scale equation to one"     (option ruleScaleEquation)
          <*> label "Eliminate variable"        (repeat ruleEliminateVar)
          <*> label "Cover up first equation"   ruleCoverUpEquation

dropEquation :: LabeledStrategy (Context (LinearSystem Expr))
dropEquation  =  cleanUpStrategy (fmap simplifySystem) $
   label "Drop equations" $
                 label "Inconsistent system (0=1)" ruleInconsistentSystem
             <|> label "Drop (0=0) equation"       ruleDropEquation

generalSolutionLinearSystem :: LabeledStrategy (Context (LinearSystem Expr))
generalSolutionLinearSystem = label "General solution to a linear system" $
   systemToEchelonWithEEO <*> backSubstitution


generalSolutionSystemWithMatrix :: LabeledStrategy (Context (Either (LinearSystem Expr) (Matrix Expr)))
generalSolutionSystemWithMatrix = label "General solution to a linear system (matrix approach)" $
       repeat (liftLeft dropEquation) 
   <*> conv1 
   <*> liftRight toReducedEchelon 
   <*> conv2 
   <*> repeat (liftLeft dropEquation)

gramSchmidtStrategy :: LabeledStrategy (Context (VectorSpace (Simplified Expr)))
gramSchmidtStrategy =
   label "Gram-Schmidt" $ repeat $ label "Iteration" $
       label "Consider next vector"   ruleNext 
   <*> label "Make vector orthogonal" (repeat (ruleNextOrthogonal <*> try ruleOrthogonal)) 
   <*> label "Normalize"              (try ruleNormalize)

vars :: Var [String]
vars = "variables" := []

conv1 :: Rule (Context (Either (LinearSystem Expr) (Matrix Expr)))
conv1 = translationToContext "Linear system to matrix" $ \c -> 
   let (m, vs) = systemToMatrix (fromContext c)
   in return $ set vars vs $ fmap (const (simplify m)) c
 
conv2 :: Rule (Context (Either (LinearSystem Expr) (Matrix Expr)))
conv2 = translationFromContext "Matrix to linear system" $ \c -> 
   let linsys = matrixToSystemWith (get vars c) (fromContext c)
   in return $ fmap (const (simplifySystem linsys)) c 
   
liftLeft :: (IsStrategy f, Lift f) => f (Context a) -> f (Context (Either a b))
liftLeft = lift $ makeLiftPair (maybeInContext . fmap isLeft) (\a _ -> fmap Left a)

liftRight :: (IsStrategy f, Lift f) => f (Context b) -> f (Context (Either a b))
liftRight = lift $ 
   makeLiftPair (maybeInContext . fmap isRight) (\b _ -> fmap Right b)

maybeInContext :: Context (Maybe a) -> Maybe (Context a)
maybeInContext c = fmap (\a -> fmap (const a) c) (fromContext c)

isLeft :: Either a b -> Maybe a
isLeft = either Just (const Nothing)

isRight :: Either a b -> Maybe b
isRight = either (const Nothing) Just

translationTo :: String -> (a -> Maybe b) -> Rule (Either a b)
translationTo s f = makeSimpleRule s (either (fmap Right . f) (const Nothing))

translationFrom :: String -> (b -> Maybe a) -> Rule (Either a b)
translationFrom s f = makeSimpleRule s (either (const Nothing) (fmap Left . f))

translationToContext :: String -> (Context a -> Maybe (Context b)) -> Rule (Context (Either a b))
translationToContext s f = makeSimpleRule s (maybe Nothing (fmap (fmap Right) . f) . maybeInContext . fmap isLeft)

translationFromContext :: String -> (Context b -> Maybe (Context a)) -> Rule (Context (Either a b))
translationFromContext s f = makeSimpleRule s (maybe Nothing (fmap (fmap Left) . f) . maybeInContext . fmap isRight)

instance Simplify a => Simplify (Matrix a) where
   simplify = fmap simplify
   
instance Simplify a => Simplify (Vector a) where
   simplify = fmap simplify
   
instance Simplify a => Simplify (VectorSpace a) where
   simplify = fmap simplify