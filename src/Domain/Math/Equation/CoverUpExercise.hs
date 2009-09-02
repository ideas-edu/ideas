module Domain.Math.Equation.CoverUpExercise (coverUpExercise) where

import Common.Context
import Common.Exercise
import Common.Strategy hiding (replicate)
import Common.Uniplate (transform)
import Control.Monad
import Data.Ratio
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Equation.Views
import Domain.Math.ExercisesDWO
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Expr.Symbols
import Domain.Math.View.Basic
import Prelude hiding (repeat)

------------------------------------------------------------
-- Exercise

coverUpExercise :: Exercise (OrList (Equation Expr))
coverUpExercise = makeExercise 
   { identifier    = "coverup"
   , domain        = "math"
   , description   = "solve an equation by covering up"
   , status        = Experimental
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = \a b -> a==b
   , equivalence   = \_ _ -> True
   , finalProperty = solvedEquations
   , ruleset       = map ignoreContext coverUpRulesOr
   , strategy      = coverUpStrategy
   , termGenerator = ExerciseList (map (OrList . return) (concat (fillInResult ++ coverUpEquations)))
   }

------------------------------------------------------------
-- Strategy and rules
   
coverUpStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
coverUpStrategy = cleanUpStrategy cleanUp $ label "Cover-up" $ 
   repeat (alternatives $ map ignoreContext coverUpRulesOr)

cleanUp :: Context (OrList (Equation Expr)) -> Context (OrList (Equation Expr))
cleanUp = fmap $ fmap $ fmap cleanUpExpr

cleanUpExpr :: Expr -> Expr
cleanUpExpr = transform (simplify (makeView f fromRational))
 where
   f (Negate a) = liftM negate (f a)
   f (Sqrt a)   = match rationalView a >>= rootedRational 2
   f (Sym s [Nat n, a]) | s == rootSymbol = do
      match rationalView a >>= rootedRational n
   f e = match rationalView e

rootedInt :: Integer -> Integer -> Maybe Integer 
rootedInt a b = do
   guard (a > 0)
   let d = fromInteger b ** (Prelude.recip (fromInteger a)) :: Double
       n = round d :: Integer
   guard (n Prelude.^ a == b)
   return n
   
rootedRational :: Integer -> Rational -> Maybe Rational 
rootedRational a r = do
   x <- rootedInt a (numerator r)
   y <- rootedInt a (denominator r)
   return (fromInteger x / fromInteger y)

------------------------------------------------------------
-- Testing

{-   
main = map test (concat (fillInResult ++ coverUpEquations))

test e = case apply coverUpStrategy (inContext (OrList [e])) of
            Just a | solvedList (fromContext a)  -> True
                   | otherwise -> error (show (e, a))
            _ -> error (show e) -}