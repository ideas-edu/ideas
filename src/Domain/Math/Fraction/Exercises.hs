module Domain.Math.Fraction.Exercises
   ( simpleFractionAddition
   ) where

import Common.Library
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Fraction.Strategies
import Domain.Math.Fraction.Rules

simpleFractionAddition :: Exercise Expr
simpleFractionAddition = makeExercise
 { status = Alpha
 , exerciseId = describe "Fraction exercise for STEPS" $ newId "arithmetic.fractions.steps"
 , parser = parseExpr
 , strategy = expandAndAdd
 , navigation   = termNavigator
 , equivalence = withoutContext (viewEquivalent rationalView)
 , extraRules = map use [gcdRule, lcmRule, expandRule, reduceRule]
 }