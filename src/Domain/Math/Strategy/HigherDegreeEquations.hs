module Domain.Math.Strategy.HigherDegreeEquations where

import Prelude hiding ((^), repeat)
import Data.List (sort, nub, (\\))
import Data.Maybe
import Common.Context
import Common.Exercise
import Common.Utils (safeHead)
import Common.Traversable
import Common.Transformation
import Common.Strategy hiding (not)
import Domain.Math.ExercisesDWO (higherDegreeEquations)
import Domain.Math.Strategy.QuadraticEquations (solvedList, cleanUpOrs)
import qualified Domain.Math.Strategy.QuadraticEquations as QE
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Symbolic
import Domain.Math.View.Basic
import Domain.Math.Data.Equation
import Control.Monad
import Domain.Math.Data.Polynomial

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
   , equivalence   = eqHD
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
powerZero = makeSimpleRule "power zero" (onceJoinM f)
 where
   f (Sym "^" [a, _] :==: Nat 0) =
      return (OrList [a :==: 0])
   f _ = Nothing

-- Factor-out variable on both sides of the equation
powerFactor :: Rule (OrList (Equation Expr))
powerFactor = makeSimpleRule "power factor" $ onceM $ onceM $ \e -> do
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
sameFactor = makeSimpleRule "same factor" $ onceJoinM $ \(lhs :==: rhs) -> do
   (b1, xs) <- match productView lhs
   (b2, ys) <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y, hasVars x ] -- equality is too strong?
   return $ OrList[ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]

-----------------------
   
allRules :: [Rule (OrList (Equation Expr))]
allRules = [powerZero, powerFactor, sameFactor] ++ QE.allRules

main :: IO ()
main = printDerivations higherDegreeEquationExercise xs 
 where xs = map (OrList . return) higherDegreeEquations
 
testAll :: IO ()
testAll = flip mapM_ higherDegreeEquations $ \eq ->
   flip mapM_ (take 1 $ derivations (unlabel $ ignoreContext equationsStrategy) (inContext $ OrList $ return eq)) $ \(a, ps) -> 
   let xs = a : map snd ps in
   flip mapM_ [ (x, y) | x <- xs, y <- xs ] $ \(x, y) -> 
   if eqHD (fromContext x) (fromContext y) then putChar '.' else print x >> print y >> error "STOP"

eqHD :: OrList (Equation Expr) -> OrList (Equation Expr) -> Bool
eqHD a b = f a == f b
 where
   f (OrList xs) = sort $ nub $ concatMap normHD xs

normHD :: Equation Expr -> [Expr]
normHD (x :==: y) = 
   case toPoly (x-y) of
      Just p  -> concatMap g $ factorize p
      Nothing -> 
         case (x, y) of 
            (Var _, e) | noVars e -> [simplify QE.squareRootView e]
            _ -> error $ show (x,y)
 where
   g :: Polynomial Rational -> [Expr]
   g p | d==0 = []
       | length (terms p) <= 1 = [0]
       | d==1 = [fromRational $ coefficient 0 p / coefficient 1 p]
       | d==2 = let [a,b,c] = [ coefficient n p | n <- [2,1,0] ]
                    discr   = b*b - 4*a*c
                in if discr < 0 then [] else 
                   map (simplify QE.squareRootView)
                   [ (-fromRational b + sqrt (fromRational discr)) / 2 * fromRational a 
                   , (-fromRational b - sqrt (fromRational discr)) / 2 * fromRational a
                   ]
       | otherwise     = error ("NOT SOLVED:" ++ show p) -- fromPoly
    where d = degree p

{- 

testje :: String
testje = concatMap f higherDegreeEquations 
 where f (x :==: y) = map g (factorizeWith candidateRoots (toPoly (x-y)))
       g p | degree p <= 2 = '.'
           | length (terms p) <= 1 = '.'
           | otherwise     = error (show p) -}

toPoly :: Expr -> Maybe (Polynomial Rational)
toPoly e = do
   (_, p) <- match QE.polyView e
   switch (fmap (match rationalView) p)