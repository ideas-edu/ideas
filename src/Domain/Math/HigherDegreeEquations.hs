module Domain.Math.HigherDegreeEquations where

import Prelude hiding ((^), repeat)
import Data.List (nub, (\\))
import Data.Maybe
import Common.Context
import Common.Exercise
import Common.Utils (safeHead)
import Common.Transformation
import Common.Strategy hiding (not)
import Domain.Math.ExercisesDWO (higherDegreeEquations)
import Domain.Math.QuadraticEquations (solvedList, cleanUpOrs, forOne, oneSide)
import qualified Domain.Math.QuadraticEquations as QE
import Domain.Math.OrList
import Domain.Math.Expr
import Domain.Math.Parser
import Domain.Math.Symbolic
import Domain.Math.Views
import Domain.Math.Equation
import Control.Monad

------------------------------------------------------------
-- Exercise

higherDegreeEquationExercise :: Exercise (OrList (Equation Expr))
higherDegreeEquationExercise = makeExercise 
   { identifier    = "higherdegree"
   , domain        = "math"
   , description   = "solve an equation (higher degree)"
   , status        = Experimental
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = (==) 
   , equivalence   = \_ _ -> True -- equality higherDegreeEquationExercise -- TO DO: what about equivalence for undecidable domains?
   , finalProperty = solvedList
   , ruleset       = map ignoreContext allRules
   , strategy      = ignoreContext equationsStrategy
   , termGenerator = ExerciseList (map (OrList . return) higherDegreeEquations)
   }

-----------------------------------------------------------
-- Strategy

equationsStrategy :: LabeledStrategy (OrList (Equation Expr))
equationsStrategy = cleanUpStrategy cleanUpOrs $
   label "higher degree" $ repeat (alternatives allRules)
 
-----------------------------------------------------------

-- A^B = 0  implies  A=0
powerZero :: Rule (OrList (Equation Expr))
powerZero = makeSimpleRuleList "power zero" (forOne f)
 where
   f (Sym "^" [a, _] :==: Nat 0) =
      return [a :==: 0]
   f _ = Nothing

-- Factor-out variable on both sides of the equation
powerFactor :: Rule (OrList (Equation Expr))
powerFactor = makeSimpleRuleList "power factor" $ forOne $ oneSide $ \e -> do
   xs <- match sumView e >>= mapM (match powerView)
   let (as, vs, ns) = unzip3 xs
       r = minimum ns
       v = variable (head vs)
       f a n = a*v^fromInteger (n-r)
   unless (length xs > 1 && length (nub vs) == 1 && r >= 1) Nothing
   -- also search for gcd constant
   case mapM (match integerView) as of 
      Just is | g > 1 -> 
         return (fromInteger g * v^fromInteger r * foldr1 (+) (zipWith f (map (fromInteger . (`div` g)) is) ns))
       where g = foldr1 gcd is
      _ -> 
         return (v^fromInteger r * build sumView (zipWith f as ns))

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Rule (OrList (Equation Expr))
sameFactor = makeSimpleRuleList "same factor" $ forOne $ \(lhs :==: rhs) -> do
   (b1, xs) <- match productView lhs
   (b2, ys) <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y ] -- equality is too strong?
   return [ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]

-----------------------
   
allRules :: [Rule (OrList (Equation Expr))]
allRules = [powerZero, powerFactor, sameFactor] ++ QE.allRules

main :: IO ()
main = printDerivations higherDegreeEquationExercise xs 
 where xs = map (OrList . return) higherDegreeEquations