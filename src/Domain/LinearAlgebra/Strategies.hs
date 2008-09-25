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
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearExpr
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Context
import Domain.LinearAlgebra.Vector

toReducedEchelon :: (Argument a, Fractional a) => LabeledStrategy (Context (Matrix a))
toReducedEchelon = label "Gaussian elimination" $ 
   forwardPass <*> backwardPass

forwardPass :: (Argument a, Fractional a) => LabeledStrategy (Context (Matrix a))
forwardPass = label "Forward pass" $ 
   repeat  $    label "Find j-th column"      ruleFindColumnJ 
           <*>  label "Exchange rows"         (try ruleExchangeNonZero)
           <*>  label "Scale row"             (try ruleScaleToOne)
           <*>  label "Zeros in j-th column"  (repeat ruleZerosFP)
           <*>  label "Cover up top row"      ruleCoverRow
  
backwardPass :: (Argument a, Fractional a) => LabeledStrategy (Context (Matrix a))
backwardPass =  label "Backward pass" $ 
   repeat  $    label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeat ruleZerosBP)

backSubstitutionSimple :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
backSubstitutionSimple = label "Back substitution with equally many variables and equations" $
       label "Cover all equations" ruleCoverAllEquations
   <*> repeat (   label "Uncover one equation"  ruleUncoverEquation
              <*> label "Scale equation to one" (try ruleScaleEquation)
              <*> label "Back Substitution"     (repeat ruleBackSubstitution)
              )

backSubstitution :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
backSubstitution = label "Back substitution" $ 
   ruleIdentifyFreeVariables <*> backSubstitutionSimple
   
systemToEchelonWithEEO :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
systemToEchelonWithEEO = label "System to Echelon Form (EEO)" $
   repeat $   dropEquation
          <|> check (not . null . remaining)
          <*> label "Exchange equations"        (try ruleExchangeEquations)
          <*> label "Scale equation to one"     (option ruleScaleEquation)
          <*> label "Eliminate variable"        (repeat ruleEliminateVar)
          <*> label "Cover up first equation"   ruleCoverUpEquation

dropEquation :: (Argument a, IsLinear a) => Strategy (Context (LinearSystem a))
dropEquation  =  label "Inconsistent system (0=1)" ruleInconsistentSystem
             <|> label "Drop (0=0) equation"       ruleDropEquation

generalSolutionLinearSystem :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
generalSolutionLinearSystem = label "General solution to a linear system" $
   systemToEchelonWithEEO <*> backSubstitution


generalSolutionSystemWithMatrix :: (Argument a, IsLinear a) => LabeledStrategy (Context (Either (LinearSystem a) (Matrix a)))
generalSolutionSystemWithMatrix = label "General solution to a linear system (matrix approach)" $
       repeat (liftLeft dropEquation) 
   <*> conv1 
   <*> liftRight toReducedEchelon 
   <*> conv2 
   <*> repeat(liftLeft dropEquation)

gramSchmidt :: Floating a => LabeledStrategy (Context [Vector a])
gramSchmidt = label "Gram-Schmidt" $ repeat $ label "Iteration" $
       label "Consider next vector"   ruleNext 
   <*> label "Make vector orthogonal" (repeat (ruleOrthogonal <*> ruleNextOrthogonal)) 
   <*> label "Normalize"              (try ruleNormalize)

vars :: Var [String]
vars = "variables" := []

conv1 :: IsLinear a => Rule (Context (Either (LinearSystem a) (Matrix a)))
conv1 = translationToContext "Linear system to matrix" $ \c -> 
   let (m, vs) = systemToMatrix (fromContext c)
   in return $ set vars vs $ fmap (const m) c
 
conv2 :: IsLinear a => Rule (Context (Either (LinearSystem a) (Matrix a)))
conv2 = translationFromContext "Matrix to linear system" $ \c -> 
   let linsys = matrixToSystemWith (get vars c) (fromContext c)
   in return $ fmap (const linsys) c 
   
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