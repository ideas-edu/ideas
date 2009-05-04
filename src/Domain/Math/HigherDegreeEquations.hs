module Domain.Math.HigherDegreeEquations where
  -- (higherDegreeEquationExercise, main) where

import Prelude hiding ((^), repeat)
import Data.List (nub, (\\))
import Data.Maybe
import Common.Context
import Common.Exercise
import qualified Text.Parsing as P
import Common.Utils (safeHead, fixpoint)
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Uniplate
import Domain.Math.ExercisesDWO (higherDegreeEquations)
import Domain.Math.QuadraticEquations (solvedList)
import Domain.Math.OrList
import Domain.Math.Expr
import Domain.Math.Parser
import Domain.Math.Symbolic
import Domain.Math.Views
import Domain.Math.Equation
import Control.Monad 
import Test.QuickCheck hiding (check, label)
import Data.Ratio

------------------------------------------------------------
-- Exercise

higherDegreeEquationExercise :: Exercise (OrList (Equation Expr))
higherDegreeEquationExercise = makeExercise 
   { identifier    = "higherdegree"
   , domain        = "math"
   , description   = "solve an equation (higher degree)"
   , status        = Experimental
   , parser        = parseOrs
   , equality      = (==) 
                     -- let f (OrList xs) = sort (map (fmap cleanUpExpr) xs)
                     -- in \a b -> a == b
   , equivalence   = \_ _ -> True -- equality higherDegreeEquationExercise -- TO DO: what about equivalence for undecidable domains?
   , finalProperty = solvedList
   , ruleset       = allRules
   , strategy      = equationsStrategy
   , termGenerator = ExerciseList (map (OrList . return) $ take 1 higherDegreeEquations)
   }
   
parseOrs :: String -> Either P.SyntaxError (OrList (Equation Expr))
parseOrs = f . P.parse pOrs . P.scanWith myScanner
 where 
   myScanner = scannerExpr 
      { P.keywordOperators = "==" : P.keywordOperators scannerExpr 
      , P.keywords = ["or"]
      }
   
   pOrs = pSepList (pEquation pExpr) (P.pKey "or")
   pSepList p q = (\x xs -> OrList (x:xs)) P.<$> p P.<*> P.pList (q P.*> p)
 
   f (e, []) = Right e
   f (_, xs) = Left $ P.ErrorMessage $ unlines $ map show xs
 
-----------------------------------------------------------
      
-- A*B = 0  implies  A=0 or B=0
productZero :: Equation Expr -> Maybe [Equation Expr]
productZero (e :==: Nat 0) | length xs > 1 = 
   return [ x :==: 0 | x <- xs ]
 where xs = maybe [] snd (match productView e)
productZero _ = Nothing

-- A^B = 0  implies  A=0
powerZero :: Equation Expr -> Maybe (Equation Expr)
powerZero (Sym "^" [a, _] :==: Nat 0) =
   return (a :==: 0)
powerZero _ = Nothing

-- A*X^I + B = C  implies  A*X^I = C - B
-- copy/paste, but it is just for demonstration
moveToRHS :: Equation Expr -> Maybe (Equation Expr)
moveToRHS (lhs :==: rhs) = do
   xs <- match sumView lhs
   unless (length xs > 1) Nothing
   case filter hasVars xs of
      [e] -> return (e :==: rhs - foldr1 (+) (xs \\ [e]))
      _   -> Nothing

powerFactor :: Expr -> Maybe Expr
powerFactor e = do
   xs <- match sumView e >>= mapM (match powerView)
   let (as, vs, ns) = unzip3 xs
       r = minimum ns
       v = variable (head vs)
       f a n = a*v^(Nat (n-r))
       Nat n = if n < 0 then negate (Nat (abs n)) else Nat n
   unless (length xs > 1 && length (nub vs) == 1 && r >= 1) Nothing
   -- also search for gcd constant
   case mapM (match integerView) as of 
      Just is | g > 1 -> 
         return (Nat g * v^Nat r * foldr1 (+) (zipWith f (map (Nat . (`div` g)) is) ns))
       where g = foldr1 gcd is
      _ -> 
         return (v^Nat r * foldr1 (+) (zipWith f as ns))

-- X^2 = A  implies  X= +/- sqrt(A)
squared :: Equation Expr -> Maybe [Equation Expr]
squared (Sym "^" [Var x, Nat 2] :==: c) | noVars c = 
   return [Var x :==: sqrt c, Var x :==: negate (sqrt c)]
squared _ = Nothing

-- A*X^i  = B  implies  X^i = B/A
divide :: Equation Expr -> Maybe (Equation Expr)
divide (lhs :==: rhs) = do
   unless (noVars rhs) Nothing
   (a, x, i) <- match powerView lhs
   when (a == 1 || i == 0) Nothing
   return (variable x ^ Nat i :==: rhs / a) 

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Expr -> Maybe Expr
niceFactors e = do
   (x, ra, rb, rc) <- match quadraticView e
   guard (ra==1)
   b <- isInt rb
   c <- isInt rc
   safeHead [ (variable x + Nat i) * (variable x + Nat j) | (i, j) <- factors c, i+j == b ]

moveToLHS :: Equation Expr -> Maybe (Equation Expr)
moveToLHS (x :==: y) = do 
   (_, _, n) <- match powerView y
   unless (n >= 1) Nothing
   return (x - y :==: 0)

abcFormula :: Equation Expr -> Maybe [Equation Expr]
abcFormula (e :==: Nat 0) = do
   (x, ra, rb, rc) <- match quadraticView e
   a <- isInt ra
   b <- isInt rb
   c <- isInt rc
   let discr = Nat (b*b - 4 * a * c)
   return [ variable x :==: (-Nat b + sqrt discr) / 2 * Nat a
          , variable x :==: (-Nat b - sqrt discr) / 2 * Nat a
          ]
abcFormula _ = Nothing

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Equation Expr -> Maybe [Equation Expr]
sameFactor (lhs :==: rhs) = do
   (b1, xs) <- match productView lhs
   (b2, ys) <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y ] -- equality is too strong?
   return [ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]

isInt :: Rational -> Maybe Integer
isInt r = do
   guard (denominator r == 1)
   return (numerator r)

-----------------------

rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10
   :: Rule (OrList (Equation Expr))
rule1  = makeSimpleRule "productZero" $ liftEqn productZero
rule2  = makeSimpleRule "powerZero"   $ liftEqn (fmap return . powerZero)
rule3  = makeSimpleRule "moveToRHS"   $ liftEqn (fmap return . moveToRHS)
rule4  = makeSimpleRule "powerFactor" $ liftExpr powerFactor
rule5  = makeSimpleRule "squared"     $ liftEqn squared
rule6  = makeSimpleRule "niceFactors" $ liftExpr niceFactors
rule7  = makeSimpleRule "moveToLHS"   $ liftEqn (fmap return . moveToLHS)
rule8  = makeSimpleRule "divide"      $ liftEqn (fmap return . divide)
rule9  = makeSimpleRule "abcFormula"  $ liftEqn abcFormula
rule10 = makeSimpleRule "sameFactor"  $ liftEqn sameFactor

liftEqn :: (Equation Expr -> Maybe [Equation Expr]) -> (OrList (Equation Expr) -> Maybe (OrList (Equation Expr)))
liftEqn f (OrList eqs) = fmap OrList (rec eqs) 
 where
   rec []     = Nothing
   rec (x:xs) = 
      case f x of
         Just ys -> return (ys ++ xs)
         Nothing -> fmap (x:) (rec xs)

liftExpr :: (Expr -> Maybe Expr) -> (OrList (Equation Expr) -> Maybe (OrList (Equation Expr)))
liftExpr f = liftEqn $ \(lhs :==: rhs) ->
   case ({- somewhereM -} f lhs, {- somewhereM -} f rhs) of
      (Just n, _) -> Just [n :==: rhs]
      (_, Just n) -> Just [lhs :==: n]
      _           -> Nothing

-----------------------

-- clean up the expression: not at all a complete list of simplifications
cleanUpExpr :: Expr -> Expr 
cleanUpExpr = fixpoint (transform (\e -> fromMaybe (step e) (basic e)))
 where
   -- plus
   step (x :+: Negate y) = x - y
   step (x :+: (Negate y :*: z)) = x - (y*z)
   -- minus
   step (x :-: (y :-: z)) = (x-y)+z
   step (0 :-: x) = Negate x
   -- negate 
   step (Negate (Nat 0))   = 0
   step (Negate (x :*: y)) = (-x)*y
   -- times
   step (x :*: (y :*: z)) | noVars x && noVars y = ((x :*: y) :*: z) -- not so nice!
   step ((x :/: y) :*: z) | y==z = x
   step (x :*: (y :/: z)) | x==z = y
   step (x :*: (y :/: z)) = (x :*: y) :/: z
   -- division
   step ((a :*: x) :/: b) | a==b && b/=0 = x
   step (Negate (x :/: y)) = (-x)/y
   -- square roots
   step (Sqrt (Nat n)) | a*a == n = Nat a
    where a = round (sqrt (fromIntegral n))
   step (Sqrt (x@(Nat _) :/: y)) = sqrt x / sqrt y -- exceptional case: sqrt (-2/-3)
   step (Sqrt (x :/: y@(Nat _))) = sqrt x / sqrt y
   -- finally, propagate constants
   step expr =
      let Nat n = if n >= 0 then Nat n else negate (Nat (abs n)) in
      case match rationalView expr of
         Just r -> fromRational r
         Nothing -> expr

   -- identities/absorbing
   basic :: Expr -> Maybe Expr
   basic (Nat 0 :+: x) = return x
   basic (x :+: Nat 0) = return x
   basic (Nat 0 :*: _) = return 0
   basic (_ :*: Nat 0) = return 0
   basic (Nat 1 :*: x) = return x
   basic (x :*: Nat 1) = return x
   basic (x :-: Nat 0) = return x
   basic (x :/: Nat 1) = return x
   basic (Nat 0 :/: _) = return 0 -- division-by-zero
   basic (Sym "^" [_, Nat 0]) = return 1
   basic (Sym "^" [x, Nat 1]) = return x
   basic (Sym "^" [Nat 0, _]) = return 0 -- except that 0^0 = 1 (by definition), or left undefined
   basic (Sym "^" [Nat 1, _]) = return 1
   basic _ = Nothing
   
-----------------------

-- The last check "removes" the unsuccessful paths. Quick solution
-- to get the derivations, but does not prevent these paths from
-- being explored by the "first" service
equationsStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
equationsStrategy = cleanUpStrategy (fmap (fmap (fmap cleanUpExpr))) $
   label "higher degree" $ repeat ( 
   alternatives allRules)
        -- <*> check (solved . fromContext)
   
allRules :: [Rule (Context (OrList (Equation Expr)))]
allRules = map liftRule [ rule1, rule2, rule3, rule4, rule5
                        , rule6, rule7, rule8, rule10, rule9] --abc last in list
 
liftRule :: Rule a -> Rule (Context a)
liftRule = lift $ makeLiftPair (return . fromContext) (fmap . const)

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))
 
q = traceStrategy (unlabel equationsStrategy) 
       (inContext (OrList [ (x-1)*(x^3 - 6*x) :==: 3*x^3 - 3*x^2]))
                                  -- (x-1)*(x^3 - 6*x) :==: 3*x^2 * (x-1) ])
 where x = variable "x"
       
main :: IO ()
main = printDerivations higherDegreeEquationExercise xs 
 where xs = map (OrList . return) (take 5 higherDegreeEquations)