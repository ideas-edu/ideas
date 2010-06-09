-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.BrokenExercises 
   ( brokenEquationExercise, normalizeBrokenExercise
   ) where

import Prelude hiding (repeat)
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Traversable
import Common.Uniplate
import Common.View
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Examples.DWO4
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Exercises (eqOrList)
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Views
import Domain.Math.Numeric.Views
import Data.Ratio

go  = checkExercise brokenEquationExercise
go2 = checkExercise normalizeBrokenExercise

brokenEquationExercise :: Exercise (OrList (Equation Expr))
brokenEquationExercise = makeExercise 
   { description  = "solve a broken equation (with a variable in a divisor)"
   , exerciseCode = makeCode "math" "brokeneq"
   , status       = Alpha -- Provisional
   , parser       = parseExprWith (pOrList (pEquation pExpr))
   , isReady      = (`belongsTo` switchView (switchView polyView))
--   , equivalence  = ??
   , similarity   = eqOrList cleanUpExpr2
   , strategy     = brokenEquationStrategy
   , navigation   = exprNavigator
   , examples     = map return (concat brokenEquations)
   }
   
normalizeBrokenExercise :: Exercise Expr
normalizeBrokenExercise = makeExercise
   { description  = "normalize a broken expression (with a variable in a divisor)"
   , exerciseCode = makeCode "math" "normbroken"
   , status       = Alpha -- Provisional
   , parser       = parseExpr
   , isReady      = isNormBroken
--   , equivalence  = ??
   , similarity   = \x y -> cleanUpExpr2 x == cleanUpExpr2 y
   , strategy     = normalizeBrokenStrategy
   , navigation   = exprNavigator
   , examples     = concat (normBroken ++ normBroken2)
   }
   
brokenEquationStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
brokenEquationStrategy = cleanUpStrategy (cleanTop (fmap (fmap cleanUpExpr2))) $
   label "Broken equation" $ repeat $ 
   (  use divisionIsZero <|> use divisionIsOne 
  <|> use sameDivisor <|> use sameDividend
  <|> use coverUpPlus <|> use coverUpMinusLeft <|> use coverUpMinusRight
  <|> use coverUpNegate) |>
   (use crossMultiply <|> use multiplyOneDiv)

normalizeBrokenStrategy :: LabeledStrategy (Context Expr)
normalizeBrokenStrategy = cleanUpStrategy (cleanTop cleanUpExpr2) $
   label "Normalize broken expression" $
   repeat (use fractionScale <|> use fractionPlus)

-- a/b = 0  iff  a=0 (and b/=0)
divisionIsZero :: Rule (Equation Expr)
divisionIsZero = makeSimpleRule "divisionIsZero" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (a, _) <- match divView lhs
   return (a :==: 0)
   
-- a/b = 1  iff  a=b (and b/=0)
divisionIsOne :: Rule (Equation Expr)
divisionIsOne = makeSimpleRule "divisionIsOne" $ \(lhs :==: rhs) -> do
   guard (rhs == 1)
   (a, b) <- match divView lhs
   return (a :==: b)

-- a/c = b/c  iff  a=b (and c/=0)
sameDivisor :: Rule (Equation Expr)
sameDivisor = makeSimpleRule "sameDivisor" $ \(lhs :==: rhs) -> do
   (a, c1) <- match divView lhs
   (b, c2) <- match divView rhs
   guard (c1==c2)
   return (a :==: b)
   
-- a/b = a/c  iff  a=0 or b=c (and b/=0 and c/=0)
sameDividend :: Rule (OrList (Equation Expr))
sameDividend = makeSimpleRule "sameDividend" $ onceJoinM $ \(lhs :==: rhs) -> do
   (a1, b) <- match divView lhs
   (a2, c) <- match divView rhs
   guard (a1==a2)
   return $ orList [a1 :==: 0, b :==: c]
   
-- a/b = c/d  iff  a*d = b*c
crossMultiply :: Rule (Equation Expr)
crossMultiply = makeSimpleRule "crossMultiply" $ \(lhs :==: rhs) -> do
   (a, b) <- match divView lhs
   (c, d) <- match divView rhs
   return (a*d :==: b*c)
   
-- a/b = c  iff  a = b*c
multiplyOneDiv :: Rule (Equation Expr)
multiplyOneDiv = makeSimpleRuleList "multiplyOneDiv" $ \(lhs :==: rhs) -> 
   f (:==:) lhs rhs ++ f (flip (:==:)) rhs lhs
 where
   f eq ab c = do 
      guard (not (c `belongsTo` divView))
      (a, b) <- matchM divView ab
      return (a `eq` (b*c))
      
-- a/c + b/c = a+b/c   (also see Numeric.Rules)
fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule "fraction plus" $ \expr -> do
   (e1, e2) <- match plusView expr
   (a, b)   <- match divView e1
   (c, d)   <- match divView e2
   guard (b == d)
   return (build divView (a+c, b))

fractionScale :: Rule Expr
fractionScale = makeSimpleRule "fraction scale" $ \expr -> do
   let subView = powerFactorViewWith rationalView
   ((a, e1), (b, e2)) <- match (plusView >>> (divView *** divView)) expr
   (x1, r1, n1) <- match subView e1
   (x2, r2, n2) <- match subView e2
   guard (x1==x2 && r1/=0 && r2/=0 && e1/=e2)
   let f r  = numerator r * denominator r
       r3   = fromIntegral (f r1 `lcm` f r2)
       m    = n1 `max` n2
       e3   = build subView (x1, r3, m)
       newa = a * build subView (x1, r3 / r1, m-n1)
       newb = b * build subView (x1, r3 / r2, m-n2)
   return $ build (plusView >>> (divView *** divView)) ((newa, e3), (newb, e3))

isNormBroken :: Expr -> Bool
isNormBroken (a :/: b) = noVarInDivisor a && noVarInDivisor b
isNormBroken e = noVarInDivisor e

noVarInDivisor :: Expr -> Bool
noVarInDivisor expr = and [ noVars a | _ :/: a <- universe expr ]