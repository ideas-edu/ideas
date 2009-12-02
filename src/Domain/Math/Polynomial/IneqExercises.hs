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
module Domain.Math.Polynomial.IneqExercises 
   ( ineqLinearExercise, ineqQuadraticExercise, ineqHigherDegreeExercise
   ) where

import Common.Context
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Uniplate (uniplate)
import Common.View
import Control.Monad
import Data.List (nub, sortBy)
import Data.Maybe (fromMaybe)
import Domain.Logic.Formula (Logic((:||:), (:&&:)))
import Domain.Math.Clipboard
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO2
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Rules 
import Domain.Math.Polynomial.Strategies
import Domain.Math.SquareRoot.Views
import Prelude hiding (repeat)
import qualified Domain.Logic.Formula as Logic

ineqLinearExercise :: Exercise (Relation Expr)
ineqLinearExercise = makeExercise 
   { description  = "solve a linear inequation"
   , exerciseCode = makeCode "math" "linineq"
   , parser       = parseWith (pRelation pExpr)
   , isReady      = solvedRelation
   , strategy     = mapRules ignoreContext ineqLinear
   , examples     = let x = Var "x"
                        extra = (x-12) / (-2) :>: (x+3)/3
                    in map (build inequalityView) (concat ineqLin1 ++ [extra])
   } 
   
ineqQuadraticExercise :: Exercise (Logic (Relation Expr))
ineqQuadraticExercise = makeExercise 
   { description  = "solve a quadratic inequation"
   , exerciseCode = makeCode "math" "quadrineq"
   , isReady      = solvedRelations
   , strategy     = ineqQuadratic
   , examples     = map (Logic.Var . build inequalityView) (concat $ ineqQuad1 ++ [ineqQuad2])
   }

ineqHigherDegreeExercise :: Exercise (Logic (Relation Expr))
ineqHigherDegreeExercise = makeExercise 
   { description  = "solve an inequation of higher degree"
   , exerciseCode = makeCode "math" "ineqhigherdegree"
   , isReady      = solvedRelations
   , strategy     = ineqHigherDegree
   , examples     = map (Logic.Var . build inequalityView) ineqHigh
   }

ineqLinear :: LabeledStrategy (Relation Expr)
ineqLinear = cleanUpStrategy (fmap cleanUpSimple) $
   label "Linear inequation" $
      label "Phase 1" (repeat (
             removeDivision
         <|> ruleMulti (ruleSomewhere distributeTimes)
         <|> ruleMulti merge))
      <*>  
      label "Phase 2" (
         try varToLeft 
         <*> try (coverUpPlus id)
         <*> try flipSign
         <*> try coverUpTimesPositive)

-- helper strategy
coverUpPlus :: (Rule (Relation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [ coverUpBinaryRule "plus" (commOp . isPlus) (-) 
   , coverUpBinaryRule "minus left" isMinus (+)
   , coverUpBinaryRule "minus right" (flipOp . isMinus) (flip (-))
   ] -- [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]
   
coverUpTimesPositive :: Rule (Relation Expr)
coverUpTimesPositive = coverUpBinaryRule "times positive" (commOp . m) (/) varConfig
 where
   m expr = do
      (a, b) <- matchM timesView expr
      r <- matchM rationalView a
      guard (r>0)
      return (a, b)
      
flipSign :: Rule (Relation Expr)
flipSign = makeSimpleRule "flip sign" $ \r -> do
   let lhs = leftHandSide r
       rhs = rightHandSide r
   guard (isNegative lhs) 
   return $ constructor (flipSides r) (neg lhs) (neg rhs)
 where
   isNegative (Negate _) = True
   isNegative expr = 
      maybe False fst (match productView expr)
 
ineqQuadratic :: LabeledStrategy (Context (Logic (Relation Expr)))
ineqQuadratic = label "Quadratic inequality" $ 
   try (liftRule (switchView (orView >>> justOneView)) turnIntoEquation) 
   <*> mapRules (liftRule (switchView orView)) quadraticStrategy
   <*> solutionInequation

ineqHigherDegree :: LabeledStrategy (Context (Logic (Relation Expr)))
ineqHigherDegree = label "Inequality of a higher degree" $ 
   try (liftRule (switchView (orView >>> justOneView)) turnIntoEquation) 
   <*> mapRules (liftRule (switchView orView)) higherDegreeStrategy
   <*> solutionInequation

justOneView :: View (OrList a) a
justOneView = makeView (f . disjunctions) return
 where
   f (Just [r]) = Just r
   f _          = Nothing

turnIntoEquation :: Rule (Context (Relation Expr))
turnIntoEquation = makeSimpleRule "turn into equation" $ withCM $ \r -> do
   guard (relationType r `elem` ineqTypes)
   addToClipboard "ineq" (toExpr r)
   return (leftHandSide r .==. rightHandSide r)
 where
   ineqTypes = 
      [LessThan, GreaterThan, LessThanOrEqualTo, GreaterThanOrEqualTo]

solutionInequation :: Rule (Context (Logic (Relation Expr)))
solutionInequation = makeSimpleRule "solution inequation" $ withCM $ \r -> do
   ineq <- lookupClipboard "ineq" >>= fromExpr
   xs <- maybeCM (matchM orView r >>= disjunctions)
   guard (not $ null xs)
   (vs, ys) <- liftM unzip $ matchM (listView (equationView >>> equationSolvedForm)) xs
   let v  = head vs
       zs = nub $ map (simplify (squareRootViewWith rationalView)) ys
   ds <- matchM (listView doubleView) zs
   guard (all (==v) vs)
   let cmpFst a b = fst a `compare` fst b
       rs = makeRanges v (sortBy cmpFst (zip ds zs))
   return $ ors [ this | (d, this) <- rs, evalIneq ineq v d ]
 where
   ors [] = Logic.F
   ors xs = foldr1 (:||:) xs
 
   makeRanges :: String -> [(Double, Expr)] -> [(Double, Logic (Relation Expr))]
   makeRanges v xs = 
      [makeLeft $ head xs]
      ++ map (uncurry makeMiddle) (zip xs (drop 1 xs))
      ++ [makeRight $ last xs]
    where
      makeLeft  (d, e) = (d-1, Logic.Var (Var v .<. e))
      makeRight (d, e) = (d+1, Logic.Var (Var v .>. e))
      makeMiddle (d1, e1) (d2, e2) = 
         ( (d1+d2)/2
         , Logic.Var (Var v .>. e1) :&&: Logic.Var (Var v .<. e2)
         )
      
   evalIneq :: Relation Expr -> String -> Double -> Bool
   evalIneq r v d = fromMaybe False $
      liftM2 (evalType (relationType r)) (use leftHandSide) (use rightHandSide)
    where
      use f = match doubleView (sub (f r))
      
      evalType tp =
         case tp of 
            EqualTo              -> (==)
            NotEqualTo           -> (/=)
            LessThan             -> (<)
            GreaterThan          -> (>)
            LessThanOrEqualTo    -> (<=)
            GreaterThanOrEqualTo -> (>=)
            Approximately        -> \a b -> abs (a-b) < 0.001
      
      sub (Var x) | x==v = Number d
      sub expr = build (map sub cs)
       where (cs, build) = uniplate expr